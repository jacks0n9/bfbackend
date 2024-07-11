mod interpreter;

#[derive(Default, Clone)]
pub struct BfContext {
    taken: Vec<MemoryRange>,
    pub code: String,
    pointer: usize,
    must_free: bool,
}

impl BfContext {
    fn reserve(&mut self, amount: usize) -> MemoryRange {
        let mut previous_end = 0;
        for range in &self.taken {
            if range.start - previous_end >= amount {
                let new_range = MemoryRange {
                    start: range.end(),
                    offset: amount,
                };
                self.push(new_range);
                return new_range;
            }
            previous_end = range.end()
        }
        let range = MemoryRange {
            start: previous_end,
            offset: amount,
        };
        self.push(range);
        range
    }
    fn push(&mut self, range: MemoryRange) {
        self.taken.push(range);
        self.taken.sort_by_key(|a| a.end())
    }
    pub fn declare_and_reserve<T>(&mut self, size: usize, data: T) -> Variable<T> {
        let our_range = self.reserve(size);
        Variable {
            var_data: data,
            pointer: our_range,
        }
    }
    pub fn declare_byte(&mut self) -> Variable<ByteData> {
        self.declare_and_reserve(
            2,
            ByteData {
                has_been_set: false,
            },
        )
    }
    pub fn declare_array(&mut self, len: usize) -> Variable<ArrayData> {
        self.declare_and_reserve(
            len + 1,
            ArrayData {
                set_cells: Vec::new(),
                data_len: len,
            },
        )
    }
    pub fn add_to_var<'a, T: MarkSet + Into<ByteRef<'a, G>>, G: 'a>(
        &mut self,
        to_add: Signedu8,
        mut byte_ref: T,
    ) {
        if to_add.value == 0 {
            return;
        }
        byte_ref.mark_set();
        let as_byte_ref: ByteRef<'a, _> = byte_ref.into();
        self.point(as_byte_ref.var.pointer.start);
        let before_add = self.clone();
        let root = (to_add.value as f64).sqrt();
        let rounded = root.round();
        let inner_add = if !to_add.negative { "+" } else { "-" }.repeat(rounded as usize);
        let square_loop = format!(
            "{}[-{}{}{}]",
            "+".repeat(rounded as usize),
            ">".repeat(as_byte_ref.data_index + 1),
            inner_add,
            "<".repeat(as_byte_ref.data_index + 1)
        );
        self.write_code(&square_loop);
        let rounded_squared = (rounded as i32).pow(2) * to_add.signum() as i32;
        let to_add_signed: i16 = to_add.into();
        let diff_from_needed = rounded_squared.abs_diff(to_add_signed.into()) as usize;
        if diff_from_needed != 0 {
            self.point_add(as_byte_ref.data_index + 1);
            let extra = if (rounded_squared * to_add.signum() as i32) < to_add_signed.into() {
                "+"
            } else {
                "-"
            }
            .repeat(diff_from_needed);
            self.write_code(&extra);
        }
        if self.code.len() - before_add.code.len() >= to_add.value.into() {
            *self = before_add;
            self.point_add(1);
            self.write_code(&if !to_add.negative { "+" } else { "-" }.repeat(to_add.value as usize))
        }
    }
    pub fn set_variable<'a, T: HasBeenSet + MarkSet + GetPointer + Into<ByteRef<'a, G>>, G: 'a>(
        &mut self,
        value: u8,
        byte_to_set: T,
    ) {
        if byte_to_set.has_been_set() {
            self.point(byte_to_set.get_pointer());
            self.write_code("[-]");
        }
        self.add_to_var(
            Signedu8 {
                negative: false,
                value,
            },
            byte_to_set,
        )
    }
    pub fn set_array(&mut self, values: &[u8], var: &mut Variable<ArrayData>) {
        let average_sqrt = ((values.iter().map(|num| (*num as f64).sqrt()).sum::<f64>())
            / values.len() as f64) as u8;
        let divided = values.iter().map(|num| num / average_sqrt);
        let remainders = values.iter().map(|num| num % average_sqrt);
        self.point(&*var);
        self.write_code(&"+".repeat(average_sqrt.into()));
        self.loop_over_cell(var.pointer.start, |ctx| {
            ctx.write_code("-");
            for factor in divided {
                ctx.point_add(1);
                ctx.write_code(&"+".repeat(factor.into()));
            }
        });
        for remainder in remainders {
            self.point_add(1);
            self.write_code(&"+".repeat(remainder.into()));
        }
    }
    fn write_code(&mut self, code: &str) {
        self.code += code
    }
    fn reserve_and_point(&mut self, amount: usize) {
        let pointer = self.reserve(amount);
        self.point(pointer.start)
    }
    fn point_add(&mut self, add: usize) {
        self.point(self.pointer + add)
    }
    fn point_sub(&mut self, sub: usize) {
        self.point(self.pointer - sub)
    }
    pub fn loop_over_cell(&mut self, to_loop_over: usize, code: impl FnOnce(&mut BfContext)) {
        self.point(to_loop_over);
        self.must_free = true;
        self.write_code("[");
        code(self);
        self.point(to_loop_over);
        self.write_code("]");
        self.must_free = false;
    }
    fn point<T: Pointable>(&mut self, location: T) {
        let location = location.get_location();
        let diff = location.abs_diff(self.pointer);
        self.write_code(&if location > self.pointer { ">" } else { "<" }.repeat(diff));
        self.pointer = location
    }
    pub fn clear_cells<T>(&mut self, var: &Variable<T>) {
        self.point(var);
        for _ in 0..var.pointer.offset {
            self.write_code("[-]");
            self.point_add(1);
        }
    }
    pub fn display_text(&mut self, text: &str) {
        let (generated, used) = bftextmaker::gen_code(text, 15);
        //-1 for loop pointer which is not needed
        let text_var = self.declare_array(used - 1);
        self.point(text_var.pointer.start);
        self.write_code(&generated);
        self.pointer = (text_var.pointer.start + text_var.pointer.offset) - 1;
        self.free_optional(text_var);
    }
    pub fn display_var<T>(&mut self, var: &Variable<T>) {
        self.point(var.pointer.start);
        for _ in 1..var.pointer.end() {
            self.point_add(1);
            self.write_code(".");
        }
    }
    pub fn display_null_terminated_array(&mut self, var: &Variable<ArrayData>) {
        self.point(var.pointer.start);
        // make sure the loop pointer is empty
        self.write_code("[-]>");
        // go until we encounter null byte
        self.write_code("[.>]");
        // return to loop pointer
        self.write_code("<[<]")
    }
    pub fn read_char<'a, T: MarkSet + Into<ByteRef<'a, G>>, G: 'a>(&mut self, mut store_to: T) {
        store_to.mark_set();
        let as_byte_ref = store_to.into();
        self.point(as_byte_ref.pointer);
        self.write_code(",")
    }
    /// Reads until a null byte or max size
    pub fn read_string(&mut self, store_to: &mut Variable<ArrayData>, max_len: u8) {}
    //>[[-<+>]>] to shift all cells over
    /// Read the data len of the array-1 characters. If your interpreter doesn't ask for input during execution, this will hang if not given enough characters
    pub fn read_n_chars(&mut self, store_to: &mut Variable<ArrayData>) {
        let amount = store_to.var_data.data_len.try_into().unwrap_or(255);
        self.set_variable(amount - 2, store_to.get_byte_ref(0));
        self.point(store_to.pointer.start + 1);
        self.write_code("[-<+>]");
        self.point_add(1);
        // read n bytes
        self.write_code(",<<[>>[>],[<]<-]");
        self.point_add(2);
        // shift all cells over one
        self.write_code("[[-<+>]>]");
        self.pointer = store_to.pointer.end() + 1
    }
    fn clone_cell(&mut self, origin: usize, destination: usize, temp_cell: usize) {
        self.loop_over_cell(origin, |ctx| {
            ctx.write_code("-");
            ctx.point(destination);
            ctx.write_code("+");
            ctx.point(temp_cell);
            ctx.write_code("+");
        });
        self.point(temp_cell);
        self.loop_over_cell(temp_cell, |ctx| {
            ctx.write_code("-");
            ctx.point(origin);
            ctx.write_code("+");
        });
    }
    pub fn clone_var<T: Clone>(&mut self, var: &Variable<T>) -> Variable<T> {
        let cloned = self.declare_and_reserve(var.pointer.offset, var.var_data.clone());
        let temp_cell = self.reserve(1).start;
        for offset in 0..var.pointer.offset {
            self.clone_cell(
                var.pointer.start + offset,
                cloned.pointer.start + offset,
                temp_cell,
            )
        }
        cloned
    }
    pub fn free<T>(&mut self, var: Variable<T>) {
        self.clear_cells(&var);
        self.taken = self
            .taken
            .iter()
            .copied()
            .filter(|range| range.start != var.pointer.start)
            .collect();
    }
    pub fn free_optional<T>(&mut self, var: Variable<T>) {
        if self.must_free {
            self.free(var);
        }
    }
    // algorithm for checking if first two cells are equal:
    /*
            ++>++<
            [->-<]decrement cells until the first one is empty
            >>>+<<< marker cell
            + set empty first cell to not cell
            >[<->>]
            >[<]
            <<
            [>>>>>>>>>++++++++>]
    */
    fn move_cell(&mut self, origin: usize, destination: usize) {
        self.loop_over_cell(origin, |ctx| {
            ctx.write_code("-");
            ctx.point(destination);
            ctx.write_code("+");
        });
    }
    pub fn do_if_compare(&mut self, condition: IfCondition, code: impl FnOnce(&mut BfContext)) {
        match condition.comparsion_type {
            ComparisonType::Equals => {
                let comparison_space: Variable<ArrayData> = self.declare_array(4);
                let left_temp = comparison_space.pointer.start + 1;
                let right_temp = comparison_space.pointer.start + 2;
                let zero = comparison_space.pointer.start + 3;
                let marker = comparison_space.pointer.start + 4;
                self.clone_cell(
                    condition.left.pointer.start + 1,
                    left_temp,
                    condition.left.pointer.start,
                );
                self.clone_cell(
                    condition.right.pointer.start + 1,
                    right_temp,
                    condition.right.pointer.start,
                );
                self.point(left_temp);
                self.loop_over_cell(left_temp, |ctx| {
                    ctx.write_code("-");
                    ctx.point(right_temp);
                    ctx.write_code("-");
                });
                self.point(marker);
                self.write_code("+");
                self.point(left_temp);
                self.write_code("+");
                self.point(right_temp);
                self.write_code("[<->>]");
                self.write_code(">[<]");
                self.pointer = zero;
                self.point(left_temp);
                self.write_code("[");
                self.write_code("-");
                code(self);
                self.point(left_temp);
                self.write_code("]");
                self.free_optional(comparison_space);
            }
            ComparisonType::LeftGreaterThanRight => {
                let mut right_temp = self.declare_byte();
                let mut left_temp = self.declare_byte();
                self.clone_cell(
                    condition.right.pointer.start + 1,
                    right_temp.pointer.start + 1,
                    condition.right.pointer.start,
                );
                self.add_to_var(Signedu8::from(1), right_temp.get_byte_ref());
                self.clone_cell(
                    condition.left.pointer.start + 1,
                    left_temp.pointer.start + 1,
                    condition.left.pointer.start,
                );
                self.add_to_var(Signedu8::from(1), left_temp.get_byte_ref());
                let mut is_empty = self.declare_byte();
                let mut is_not_empty = self.declare_byte();
                self.set_variable(1, is_not_empty.get_byte_ref());
                self.set_variable(2, is_empty.get_byte_ref());
                self.loop_over_cell(is_not_empty.pointer.start + 1, |ctx| {
                    ctx.add_to_var(
                        Signedu8 {
                            negative: true,
                            value: 1,
                        },
                        left_temp.get_byte_ref(),
                    );
                    ctx.do_if_nonzero(&left_temp, |ctx| {
                        ctx.add_to_var(
                            Signedu8 {
                                negative: true,
                                value: 1,
                            },
                            is_empty.get_byte_ref(),
                        );
                    });
                    ctx.add_to_var(
                        Signedu8 {
                            negative: true,
                            value: 1,
                        },
                        right_temp.get_byte_ref(),
                    );
                    ctx.do_if_nonzero(&right_temp, |ctx| {
                        ctx.add_to_var(
                            Signedu8 {
                                negative: true,
                                value: 1,
                            },
                            is_empty.get_byte_ref(),
                        );
                    });
                    ctx.do_if_nonzero(&is_empty, |ctx| {
                        ctx.set_variable(0, is_not_empty.get_byte_ref());
                    });
                    ctx.set_variable(2, is_empty.get_byte_ref());
                });
                self.do_if_nonzero(&left_temp, code);
            }
            ComparisonType::LeftLessThanRight => todo!(),
            ComparisonType::NotEquals => {
                let comparison_space: Variable<ArrayData> = self.declare_array(2);
                let temp_cell = self.reserve(1);
                let left_temp = comparison_space.pointer.start + 1;
                let right_temp = comparison_space.pointer.start + 2;
                self.clone_cell(condition.left.pointer.start + 1, left_temp, temp_cell.start);
                self.clone_cell(
                    condition.right.pointer.start + 1,
                    right_temp,
                    temp_cell.start,
                );
                self.loop_over_cell(left_temp, |ctx| {
                    ctx.write_code("-");
                    ctx.point(right_temp);
                    ctx.write_code("-");
                });
                self.point(right_temp);
                self.write_code("[[-]");
                code(self);
                self.point(right_temp);
                self.write_code("]");
                self.free_optional(comparison_space);
            }
        }
    }
    pub fn do_if_nonzero(&mut self, var: &Variable<ByteData>, code: impl FnOnce(&mut BfContext)) {
        let zero_cell = self.declare_byte();
        self.point(var.pointer.start);
        self.write_code("+");
        self.point_add(1);
        self.write_code("[");
        code(self);
        self.point(zero_cell.pointer.start + 1);
        self.write_code("]");
        self.point_sub(1);
        self.write_code("[");
        // drain the loop pointer completely in case there were leftover data in it
        self.pointer = var.pointer.start;
        self.point(zero_cell.pointer.start);
        self.write_code("]");
        self.point(var.pointer.start);
        self.write_code("[-]");
    }
}
pub struct IfCondition<'a> {
    left: &'a Variable<ByteData>,
    right: &'a Variable<ByteData>,
    comparsion_type: ComparisonType,
}
pub enum ComparisonType {
    Equals,
    NotEquals,
    LeftGreaterThanRight,
    LeftLessThanRight,
}
pub struct ByteRef<'a, T> {
    data_index: usize,
    pointer: usize,
    var: &'a mut Variable<T>,
}
pub trait MarkSet {
    fn mark_set(&mut self);
}
pub trait HasBeenSet {
    fn has_been_set(&self) -> bool;
}
pub trait GetPointer {
    fn get_pointer(&self) -> usize;
}
impl<T> GetPointer for ByteRef<'_, T> {
    fn get_pointer(&self) -> usize {
        self.pointer
    }
}
impl HasBeenSet for ByteRef<'_, ByteData> {
    fn has_been_set(&self) -> bool {
        self.var.var_data.has_been_set
    }
}
impl MarkSet for ByteRef<'_, ByteData> {
    fn mark_set(&mut self) {
        self.var.var_data.has_been_set = true
    }
}
impl HasBeenSet for ByteRef<'_, ArrayData> {
    fn has_been_set(&self) -> bool {
        self.var.var_data.set_cells.contains(&self.data_index)
    }
}
impl MarkSet for ByteRef<'_, ArrayData> {
    fn mark_set(&mut self) {
        self.var.var_data.set_cells.push(self.data_index)
    }
}
#[derive(Clone)]
pub struct Variable<T> {
    var_data: T,
    pointer: MemoryRange,
}
#[derive(Clone)]
pub struct ByteData {
    has_been_set: bool,
}
#[derive(Clone)]
pub struct ArrayData {
    set_cells: Vec<usize>,
    data_len: usize,
}

impl Variable<ByteData> {
    pub fn get_byte_ref(&mut self) -> ByteRef<ByteData> {
        ByteRef {
            pointer: self.pointer.start + 1,
            data_index: 0,
            var: self,
        }
    }
}
impl Variable<ArrayData> {
    pub fn get_byte_ref(&mut self, data_index: usize) -> ByteRef<ArrayData> {
        ByteRef {
            pointer: self.pointer.start + 1 + data_index,
            data_index,
            var: self,
        }
    }
}
trait Pointable {
    fn get_location(&self) -> usize;
}
impl Pointable for usize {
    fn get_location(&self) -> usize {
        *self
    }
}
impl<T> Pointable for &Variable<T> {
    fn get_location(&self) -> usize {
        self.pointer.start
    }
}
#[derive(Clone, Copy)]
struct MemoryRange {
    /// First usable memory is this cell
    start: usize,
    /// Last usable memory is (start+offset)-1
    offset: usize,
}
impl MemoryRange {
    fn end(&self) -> usize {
        self.start + self.offset
    }
}
#[derive(Clone, Copy)]
pub struct Signedu8 {
    pub negative: bool,
    value: u8,
}

impl Signedu8 {
    fn signum(&self) -> i8 {
        if self.value == 0 {
            return 0;
        }
        if self.negative {
            -1
        } else {
            1
        }
    }
}
impl Into<i16> for Signedu8 {
    fn into(self) -> i16 {
        let sign: i16 = self.signum().into();
        let value: i16 = self.value.into();
        sign * value
    }
}
impl From<u8> for Signedu8 {
    fn from(value: u8) -> Self {
        Signedu8 {
            negative: false,
            value,
        }
    }
}
#[cfg(test)]
mod test {
    use super::*;
    fn test_add_value(value: u8) {
        let mut ctx = BfContext::default();
        let mut testing = ctx.declare_byte();
        let byte_ref = testing.get_byte_ref();
        let pointer = byte_ref.pointer;
        ctx.add_to_var(Signedu8::from(value), byte_ref);
        let code = ctx.code;
        let mut run = interpreter::BfInterpreter::new_with_code(code);
        run.run(&mut BlankIO, &mut BlankIO).unwrap();
        assert_eq!(run.cells[pointer], value);
    }
    #[test]
    fn test_add() {
        for i in 0..=255 {
            test_add_value(i)
        }
    }
    #[test]
    fn test_print() {
        let mut ctx = BfContext::default();
        let test_str = "The quick brown fox jumps over the lazy DOG1234567890";
        ctx.display_text(test_str);
        let mut writer: Vec<u8> = Vec::new();
        let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
        run.run(&mut writer, &mut BlankIO).unwrap();
        assert_eq!(writer, test_str.as_bytes())
    }
    #[test]
    fn test_equals() {
        let mut ctx = BfContext::default();
        let value = 2;
        let mut var1 = ctx.declare_byte();
        ctx.set_variable(3, var1.get_byte_ref());
        let mut var2 = ctx.declare_byte();
        ctx.set_variable(3, var2.get_byte_ref());
        let mut is_good = ctx.declare_byte();
        let byte_ref = is_good.get_byte_ref();
        let pointer = byte_ref.pointer;
        ctx.do_if_compare(
            IfCondition {
                left: &var1,
                right: &var2,
                comparsion_type: ComparisonType::Equals,
            },
            |ctx| {
                ctx.add_to_var(Signedu8::from(value), byte_ref);
            },
        );
        println!("{}", &ctx.code);
        let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
        run.run(&mut BlankIO, &mut BlankIO).unwrap();
        println!("{pointer}");
        println!("{:?}", &run.cells[..20]);
        assert_eq!(value, run.cells[pointer])
    }
    #[test]
    fn test_move_cell() {
        let mut ctx = BfContext::default();
        let value = 6;
        let mut var1 = ctx.declare_byte();
        ctx.add_to_var(Signedu8::from(value), var1.get_byte_ref());
        let var2 = ctx.declare_byte();
        ctx.move_cell(var1.pointer.start + 1, var2.pointer.start + 1);
        let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
        run.run(&mut BlankIO, &mut BlankIO).unwrap();
        assert_eq!(run.cells[var1.pointer.start + 1], 0);
        assert_eq!(run.cells[var2.pointer.start + 1], value);
    }
    #[test]
    fn test_left_greater_than_right() {
        let test_values = [
            (5, 10),
            (6, 4),
            (10, 10),
            (255, 255),
            (0, 0),
            (1, 0),
            (0, 1),
        ];
        let value = 39;
        for test_value in test_values {
            let mut ctx = BfContext::default();
            let mut left = ctx.declare_byte();
            ctx.set_variable(test_value.0, left.get_byte_ref());
            let mut right = ctx.declare_byte();
            ctx.set_variable(test_value.1, right.get_byte_ref());
            let mut should_be_set = ctx.declare_byte();
            ctx.do_if_compare(
                IfCondition {
                    left: &left,
                    right: &right,
                    comparsion_type: ComparisonType::LeftGreaterThanRight,
                },
                |ctx| {
                    ctx.set_variable(value, should_be_set.get_byte_ref());
                },
            );
            println!("{}", &ctx.code);
            let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
            run.run(&mut BlankIO, &mut BlankIO).unwrap();
            let what_cell_was_set_to = run.cells[should_be_set.pointer.start + 1];
            println!("test value: {},{}", test_value.0, test_value.1);
            if test_value.0 > test_value.1 {
                assert_eq!(what_cell_was_set_to, value)
            } else {
                println!("for zero");
                assert_eq!(what_cell_was_set_to, 0)
            }
        }
    }

    struct BlankIO;
    impl std::io::Write for BlankIO {
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
            Ok(buf.len())
        }

        fn flush(&mut self) -> std::io::Result<()> {
            Ok(())
        }
    }

    impl std::io::Read for BlankIO {
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
            Ok(buf.len())
        }
    }
}
