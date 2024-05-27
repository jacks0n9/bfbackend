use thiserror::Error;
#[derive(Default)]
pub struct BfContext {
    taken: Vec<MemoryRange>,
    loops: Vec<Loop>,
    pub code: String,
    pointer: usize,
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
        let as_byte_ref = byte_ref.into();
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
    pub fn set_variable<'a, T: Clone + HasBeenSet + MarkSet + Into<ByteRef<'a, G>>, G: 'a>(
        &mut self,
        to_set: u8,
        index: T,
    ) {
        let has_been_set = index.has_been_set();
        let og = index.clone();
        let as_byte_ref = index.into();
        if has_been_set {
            self.point(as_byte_ref.pointer);
            self.write_code("[-]");
        }
        self.add_to_var(to_set as i16, og)
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
        })
    }
    pub fn end_loop(&mut self) -> Result<(), MismatchedBracketsError> {
        let loop_we_are_closing = self.loops.pop().ok_or(MismatchedBracketsError)?;
        self.point(loop_we_are_closing.original_pointer);
        self.write_code("]");
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
        self.reserve_and_point(used);
        self.write_code(&generated);
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

pub struct Variable<T> {
    dynamic: bool,
    var_data: T,
    pointer: MemoryRange,
}

pub struct ByteData {
    has_been_set: bool,
}
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
        let mut store_to = ctx.declare_array(10);
        let bytes = ["sussy".as_bytes(), &[0]].concat();
        ctx.set_array(&bytes, &mut store_to);
        ctx.display_null_terminated_array(&store_to);

        println!("{}", ctx.code);
    }
}
