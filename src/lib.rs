use std::borrow::BorrowMut;
#[derive(Default)]
pub struct BfContext {
    taken: Vec<MemoryRange>,
    pub code: String,
    pointer: usize,
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
    pub fn declare_var(&mut self, var_type: VarDeclareArgsType) -> Variable {
        //loop pointer
        let mut amount = 1;
        match var_type {
            VarDeclareArgsType::Array(size) => amount += size,
            VarDeclareArgsType::Byte => amount += 1,
        }
        let var_type = match var_type {
            VarDeclareArgsType::Byte => VarType::Byte {
                has_been_set: false,
            },
            VarDeclareArgsType::Array(size) => VarType::ByteArray {
                set_cells: Vec::new(),
                data_len: size,
            },
        };
        let pointer = self.reserve(amount);
        Variable {
            var_type,
            pointer,
            dynamic: false,
        }
    }
    pub fn add_to_var(&mut self, to_add: i16, index: ByteRef) {
        if to_add == 0 {
            return;
        }
        let root = (to_add.abs() as f64).sqrt();
        let rounded = root.round();
        self.point(index.var.pointer.start);
        let inner_add = if to_add.is_positive() { "+" } else { "-" }.repeat(rounded as usize);

        let square_loop = format!(
            "{}[-{}{}{}]",
            "+".repeat(rounded as usize),
            ">".repeat(index.data_index + 1),
            inner_add,
            "<".repeat(index.data_index + 1)
        );
        self.write_code(&square_loop);
        let rounded_squared = (rounded as i32).pow(2) * to_add.signum() as i32;
        let diff_from_needed = rounded_squared.abs_diff(to_add.into()) as usize;
        if diff_from_needed != 0 {
            self.point_add(index.data_index + 1);
            let extra = if (rounded_squared * to_add.signum() as i32) < to_add.into() {
                "+"
            } else {
                "-"
            }
            .repeat(diff_from_needed);
            self.write_code(&extra);
        }
        let var_mut = index.var;
        let var_type = var_mut.var_type.borrow_mut();
        match var_type {
            VarType::Byte {
                ref mut has_been_set,
            } => *has_been_set = true,
            VarType::ByteArray {
                ref mut set_cells, ..
            } => set_cells.push(index.data_index),
        }
    }
    pub fn set_variable(&mut self, to_set: u8, index: ByteRef) {
        if index.has_been_set() {
            self.point(index.pointer);
            self.write_code("[-]");
        }
        self.add_to_var(to_set as i16, index)
    }
    pub fn set_array(&mut self, values: &[u8], var: &mut Variable) {
        let average_sqrt=((values.iter().map(|num|(*num as f64).sqrt()).sum::<f64>())/values.len() as f64) as u8;
        let divided=values.iter().map(|num|num/average_sqrt);
        let remainders=values.iter().map(|num|num%average_sqrt);
        self.point(var.pointer.start);
        self.write_code(&"+".repeat(average_sqrt.into()));
        self.write_code("[-");
        for factor in divided{
            self.point_add(1);
            self.write_code(&"+".repeat(factor.into()));
        }
        self.point(var.pointer.start);
        self.write_code("]");
        for remainder in remainders{
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
        self.point(self.pointer -sub)
    }
    
    fn point<T: Pointable>(&mut self, location: T) {
        let location = location.get_location();
        let diff = location.abs_diff(self.pointer);
        self.write_code(&if location > self.pointer { ">" } else { "<" }.repeat(diff));
        self.pointer = location
    }
    pub fn display_text(&mut self, text: &str) {
        let (generated, used) = bftextmaker::gen_code(text, 15);
        self.reserve_and_point(used);
        self.write_code(&generated);
    }
    pub fn display_var(&mut self, var: &Variable) {
        self.point(var.pointer.start);
        for _ in 1..var.pointer.end() {
            self.point_add(1);
            self.write_code(".");
        }
    }
}
pub struct ByteRef<'a> {
    data_index: usize,
    pointer: usize,
    var: &'a mut Variable,
}
impl ByteRef<'_> {
    fn has_been_set(&self) -> bool {
        match &self.var.var_type {
            VarType::Byte { has_been_set } => *has_been_set,
            VarType::ByteArray { set_cells, .. } => set_cells.contains(&self.data_index),
        }
    }
}
pub enum VarDeclareArgsType {
    Array(usize),
    Byte,
}
pub struct Variable {
    dynamic: bool,
    var_type: VarType,
    pointer: MemoryRange,
}
enum VarType {
    Byte {
        has_been_set: bool,
    },
    ByteArray {
        set_cells: Vec<usize>,
        data_len: usize,
    },
}
impl Variable {
    fn get_byte_ref(&mut self, data_index: usize) -> ByteRef {
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
impl Pointable for &Variable {
    fn get_location(&self) -> usize {
        self.pointer.start
    }
}
#[derive(Clone, Copy)]
struct MemoryRange {
    // First usable memory is this cell
    start: usize,
    // Last usable memory is (start+offset)-1
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
        let mut var = ctx.declare_var(VarDeclareArgsType::Array(25));
        ctx.set_array("when the imposter is sus".as_bytes(), &mut var);
        ctx.display_var(&var);
        println!("{}", ctx.code);
    }
}
