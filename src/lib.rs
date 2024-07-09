use thiserror::Error;
#[derive(Default)]
pub struct BfContext {
    taken: Vec<MemoryRange>,
    loops: Vec<Loop>,
    pub code: String,
    pointer: usize,
    must_free: bool,
}
#[derive(Clone)]
struct Loop {
    original_pointer: usize,
}
#[derive(Error, Debug)]
#[error("Unclosed brackets or tried to close brackets with no matching opening bracket")]
pub struct MismatchedBracketsError;
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
            dynamic: false,
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
        to_add: i16,
        mut byte_ref: T,
    ) {
        if to_add == 0 {
            return;
        }
        let root = (to_add.abs() as f64).sqrt();
        let rounded = root.round();
        byte_ref.mark_set();
        let as_byte_ref: ByteRef<'a, _> = byte_ref.into();
        self.point(as_byte_ref.var.pointer.start);
        let inner_add = if to_add.is_positive() { "+" } else { "-" }.repeat(rounded as usize);

        let square_loop = format!(
            "{}[-{}{}{}]",
            "+".repeat(rounded as usize),
            ">".repeat(as_byte_ref.data_index + 1),
            inner_add,
            "<".repeat(as_byte_ref.data_index + 1)
        );
        self.write_code(&square_loop);
        let rounded_squared = (rounded as i32).pow(2) * to_add.signum() as i32;
        let diff_from_needed = rounded_squared.abs_diff(to_add.into()) as usize;
        if diff_from_needed != 0 {
            self.point_add(as_byte_ref.data_index + 1);
            let extra = if (rounded_squared * to_add.signum() as i32) < to_add.into() {
                "+"
            } else {
                "-"
            }
            .repeat(diff_from_needed);
            self.write_code(&extra);
        }
    }
    pub fn set_variable<'a, T: HasBeenSet + MarkSet + GetPointer + Into<ByteRef<'a, G>>, G: 'a>(
        &mut self,
        to_set: u8,
        index: T,
    ) {
        if index.has_been_set() {
            self.point(index.get_pointer());
            self.write_code("[-]");
        }
        self.add_to_var(to_set as i16, index)
    }
    pub fn set_array(&mut self, values: &[u8], var: &mut Variable<ArrayData>) {
        let average_sqrt = ((values.iter().map(|num| (*num as f64).sqrt()).sum::<f64>())
            / values.len() as f64) as u8;
        let divided = values.iter().map(|num| num / average_sqrt);
        let remainders = values.iter().map(|num| num % average_sqrt);
        self.point(&*var);
        self.write_code(&"+".repeat(average_sqrt.into()));
        self.start_loop();
        self.write_code("-");
        for factor in divided {
            self.point_add(1);
            self.write_code(&"+".repeat(factor.into()));
        }
        self.point(&*var);
        let _ = self.end_loop();
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
    pub fn start_loop(&mut self) {
        self.write_code("[");
        self.loops.push(Loop {
            original_pointer: self.pointer,
        });
        self.must_free = true
    }
    pub fn end_loop(&mut self) -> Result<(), MismatchedBracketsError> {
        let loop_we_are_closing = self.loops.pop().ok_or(MismatchedBracketsError)?;
        self.point(loop_we_are_closing.original_pointer);
        self.write_code("]");
        if self.loops.is_empty() {
            self.must_free = false;
        }
        Ok(())
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
        self.point(origin);
        self.start_loop();
        self.write_code("-");
        self.point(destination);
        self.write_code("+");
        self.point(temp_cell);
        self.write_code("+");
        let _ = self.end_loop();
        self.point(temp_cell);
        self.start_loop();
        self.write_code("-");
        self.point(origin);
        self.write_code("+");
        let _ = self.end_loop();
    }
    pub fn clone<T: Clone>(&mut self, var: &Variable<T>) -> Variable<T> {
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
    pub fn do_if_compare(&mut self, condition: IfCondition, code: impl Fn(&mut BfContext)) {
        match condition.comparsion_type {
            ComparisonType::Equals => {
                let comparison_space: Variable<ArrayData> = self.declare_array(4);
                let temp_cell = self.reserve(1);
                let left_temp = comparison_space.pointer.start + 1;
                let right_temp = comparison_space.pointer.start + 2;
                let zero = comparison_space.pointer.start + 3;
                let marker = comparison_space.pointer.start + 4;
                self.clone_cell(condition.left.pointer.start + 1, left_temp, temp_cell.start);
                self.clone_cell(
                    condition.right.pointer.start + 1,
                    right_temp,
                    temp_cell.start,
                );
                self.point(left_temp);
                self.start_loop();
                self.write_code("-");
                self.point(right_temp);
                self.write_code("-");
                let _ = self.end_loop();
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
            ComparisonType::LeftGreaterThanRight => todo!(),
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
                self.point(left_temp);
                self.start_loop();
                self.write_code("-");
                self.point(right_temp);
                self.write_code("-");
                let _ = self.end_loop();
                self.point(right_temp);
                self.write_code("[[-]");
                code(self);
                self.point(right_temp);
                self.write_code("]");
                self.free_optional(comparison_space);
            }
        }
    }
    pub fn do_if_nonzero(&mut self, var: &Variable<ByteData>, code: impl Fn(&mut BfContext)) {
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
struct BfFunction {}
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
    dynamic: bool,
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

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test_add() {
        let mut ctx = BfContext::default();
        let mut answer = ctx.declare_byte();
        ctx.set_variable(5, answer.get_byte_ref());
        ctx.do_if_nonzero(&answer, |ctx| {});
        println!("{}", ctx.code);
    }
}
