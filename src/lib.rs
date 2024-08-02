use std::collections::HashMap;

mod interpreter;

#[derive(Default, Clone)]
pub struct BfContext {
    taken: Vec<MemoryRange>,
    pub code: String,
    pointer: usize,
    must_free: usize,
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
    pub fn add_to_var<'a, T>(&mut self, byte_to_set: &mut MutableByteRef<'a, T>, to_add: Signedu8)
    where
        ByteRef<'a, T>: MarkSet,
    {
        if to_add.value == 0 {
            return;
        }
        byte_to_set.mark_set();
        self.point(byte_to_set.var.pointer.start);
        let before_add = self.clone();
        let root = (to_add.value as f64).sqrt();
        let rounded = root.round();
        let inner_add = if !to_add.negative { "+" } else { "-" }.repeat(rounded as usize);
        let square_loop = format!(
            "{}[-{}{}{}]",
            "+".repeat(rounded as usize),
            ">".repeat(byte_to_set.data_index + 1),
            inner_add,
            "<".repeat(byte_to_set.data_index + 1)
        );
        self.write_code(&square_loop);
        let rounded_squared = (rounded as i32).pow(2) * to_add.signum() as i32;
        let as_i16:i16=to_add.into();
        let diff_from_needed = rounded_squared.abs_diff(as_i16.into()) as usize;
        if diff_from_needed != 0 {
            self.point_add(byte_to_set.data_index + 1);
            let extra = if (rounded_squared) < as_i16.into() {
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
    pub fn set_var<'a, T>(&mut self, byte_to_set: &mut MutableByteRef<'a, T>, value: u8)
    where
        ByteRef<'a, T>: HasBeenSet + Pointable + MarkSet,
    {
        if byte_to_set.has_been_set() {
            self.point(byte_to_set.get_pointer());
            self.write_code("[-]");
        }
        self.add_to_var(
            byte_to_set,
            Signedu8 {
                negative: false,
                value,
            },
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
    pub fn write_code(&mut self, code: &str) {
        self.code += code
    }
    pub fn point_add(&mut self, add: usize) {
        self.point(self.pointer + add)
    }
    fn point_sub(&mut self, sub: usize) {
        self.point(self.pointer - sub)
    }
    pub fn loop_over_cell(&mut self, to_loop_over: usize, code: impl FnOnce(&mut BfContext)) {
        self.point(to_loop_over);
        self.must_free += 1;
        self.write_code("[");
        code(self);
        self.point(to_loop_over);
        self.write_code("]");
        self.must_free -= 1;
    }
    pub fn point<T: Pointable>(&mut self, location: T) {
        let location = location.get_pointer();
        let diff = location.abs_diff(self.pointer);
        self.write_code(&if location > self.pointer { ">" } else { "<" }.repeat(diff));
        self.pointer = location
    }
    pub fn clear_cells<T: GetRange>(&mut self, var: &T) {
        let range = var.get_range();
        self.point(range.start);
        for _ in 0..range.offset {
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
    pub fn read_char<'a, T>(&mut self, mut store_to: ByteRef<'a, T>) where ByteRef<'a, T>: MarkSet{
        store_to.mark_set();
        self.point(store_to.pointer);
        self.write_code(",")
    }
    /// Reads until a null byte or max capacity of the store_to variable, which is its pointers offset minus 2.
    /// One byte is reserved for the null byte and the other byte is for an empty byte at the start used for navigating the string efficiently
    /// String will be null-terminated, but data after the null-byte is not guaranteed to be zero, even if it is within the limits of the variable
    pub fn read_string(&mut self, store_to: &mut Variable<ArrayData>) {
        self.point((store_to.pointer.start + store_to.pointer.offset) - 1);
        self.write_code("-");
        self.point(store_to.pointer.start);
        // fill up cells with ones and return to store_to.pointer.start
        self.write_code(">+[>+]<[<]");
        // read cells until we encounter null byte
        self.write_code(">[,>]<[<]");
    }
    //>[[-<+>]>] to shift all cells over
    /// Read the data len of the array-1 characters. If your interpreter doesn't ask for input during execution, this will hang if not given enough characters
    pub fn read_n_chars(&mut self, store_to: &mut Variable<ArrayData>) {
        let amount = store_to.var_data.data_len.try_into().unwrap_or(255);
        self.set_var(&mut store_to.get_byte_ref(0), amount - 2);
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
    pub fn free<T: GetRange>(&mut self, to_free: T) {
        let range = to_free.get_range();
        self.clear_cells(&to_free);
        self.taken = self
            .taken
            .iter()
            .copied()
            .filter(|filter_range| filter_range.start != range.start)
            .collect();
    }
    pub fn free_optional<T: GetRange>(&mut self, to_free: T) {
        if self.must_free != 0 {
            self.free(to_free);
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
    // TODO: accept byteref
    pub fn move_cell(&mut self, origin: usize, destination: usize) {
        self.loop_over_cell(origin, |ctx| {
            ctx.write_code("-");
            ctx.point(destination);
            ctx.write_code("+");
        });
    }
    pub fn do_if_left_greater_than_right(
        &mut self,
        left: Variable<ByteData>,
        right: Variable<ByteData>,
        code: impl FnOnce(&mut BfContext)
    ){
        let no_else: Option<fn(&mut BfContext)>=None;
        self.do_if_left_greater_than_right_with_else(left, right, code, no_else)
    }
    pub fn do_if_left_greater_than_right_with_else(
        &mut self,
        mut left: Variable<ByteData>,
        mut right: Variable<ByteData>,
        code: impl FnOnce(&mut BfContext),
        else_if_opposite: Option<impl FnOnce(&mut BfContext)>
    ) {
        self.add_to_var(&mut right.get_byte_ref(), Signedu8::from(1));
        self.add_to_var(&mut left.get_byte_ref(), Signedu8::from(1));
        let mut is_empty = self.declare_byte();
        let mut is_not_empty = self.declare_byte();
        self.set_var(&mut is_not_empty.get_byte_ref(), 1);
        self.set_var(&mut is_empty.get_byte_ref(), 2);
        self.loop_over_cell(is_not_empty.pointer.start + 1, |ctx| {
            ctx.add_to_var(
                &mut left.get_byte_ref(),
                Signedu8 {
                    negative: true,
                    value: 1,
                },
            );
            ctx.do_if_nonzero(&left, |ctx| {
                ctx.add_to_var(
                    &mut is_empty.get_byte_ref(),
                    Signedu8 {
                        negative: true,
                        value: 1,
                    },
                );
            });
            ctx.add_to_var(
                &mut right.get_byte_ref(),
                Signedu8 {
                    negative: true,
                    value: 1,
                },
            );
            ctx.do_if_nonzero(&right, |ctx| {
                ctx.add_to_var(
                    &mut is_empty.get_byte_ref(),
                    Signedu8 {
                        negative: true,
                        value: 1,
                    },
                );
            });
            ctx.do_if_nonzero_mut(is_empty.get_byte_ref(), |ctx| {
                ctx.set_var(&mut is_not_empty.get_byte_ref(), 0);
            });
            ctx.set_var(&mut is_empty.get_byte_ref(), 2);
        });
        self.do_if_nonzero_mut(left.get_byte_ref(), code);
        if let Some(other_code)=else_if_opposite{
            self.do_if_nonzero_mut(right.get_byte_ref(), other_code);
        }
        self.free_optional(is_empty);
        self.free_optional(is_not_empty);
    }
    pub fn do_if_left_less_than_right(
        &mut self,
        left: Variable<ByteData>,
        right: Variable<ByteData>,
        code: impl FnOnce(&mut BfContext),
    ) {
        self.do_if_left_greater_than_right(right, left, code);
    }
    pub fn do_if_equal(
        &mut self,
        left: &Variable<ByteData>,
        right: &Variable<ByteData>,
        code: impl FnOnce(&mut BfContext),
    ) {
        let comparison_space: Variable<ArrayData> = self.declare_array(4);
        let left_temp = comparison_space.pointer.start + 1;
        let right_temp = comparison_space.pointer.start + 2;
        let zero = comparison_space.pointer.start + 3;
        let marker = comparison_space.pointer.start + 4;
        self.clone_cell(left.pointer.start + 1, left_temp, left.pointer.start);
        self.clone_cell(right.pointer.start + 1, right_temp, right.pointer.start);
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
    pub fn do_if_not_equal(
        &mut self,
        left: &Variable<ByteData>,
        right: &Variable<ByteData>,
        code: impl FnOnce(&mut BfContext),
    ) {
        let comparison_space: Variable<ArrayData> = self.declare_array(2);
        let temp_cell = self.reserve(1);
        let left_temp = comparison_space.pointer.start + 1;
        let right_temp = comparison_space.pointer.start + 2;
        self.clone_cell(left.pointer.start + 1, left_temp, temp_cell.start);
        self.clone_cell(right.pointer.start + 1, right_temp, temp_cell.start);
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
    /// Execute code if the variable's data is non-zero
    /// This requires a &Variable<ByteData> to be passed in rather than a ByteRef
    /// This is because this code is dependent on there being an extra cell directly to the left of the variable being checked
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
    /// Same as do_if_nonzero, but generates less code by mutating the byte you are checking
    pub fn do_if_nonzero_mut<T>(
        &mut self,
        byte_to_check: MutableByteRef<'_, T>,
        code: impl FnOnce(&mut BfContext),
    ) {
        self.loop_over_cell(byte_to_check.pointer, |ctx| {
            ctx.write_code("[-]");
            code(ctx)
        })
    }
    pub fn do_if_zero(&mut self, var: &Variable<ByteData>, code: impl FnOnce(&mut BfContext)) {
        let mut to_invert = self.declare_byte();
        self.add_to_var(&mut to_invert.get_byte_ref(), Signedu8::from(1));
        self.do_if_nonzero(var, |ctx| {
            ctx.point(to_invert.pointer.start + 1);
            ctx.write_code("-");
        });
        self.do_if_nonzero_mut(to_invert.get_byte_ref(), code);
        self.free_optional(to_invert);
    }
    pub fn match_num(&mut self, var: Variable<ByteData>) -> MatchBuilder<'_> {
        self.point(var.pointer.start);
        MatchBuilder {
            ctx: self,
            var,
            codes: HashMap::new(),
        }
    }
    /// Moves origin into destination, not emptying destination and esentially adding the two values together.
    pub fn move_byte<'a, 'b, A, B>(
        &mut self,
        origin: &mut ByteRef<'a, A>,
        destination: &mut ByteRef<'b, B>,
    ) where
        ByteRef<'a, A>: MarkSet,
        ByteRef<'b, B>: MarkSet,
    {
        origin.mark_set();
        destination.mark_set();
        self.move_cell(origin.pointer, destination.pointer);
    }
    /// Multiplies num1 by num2 and puts the result into output.
    /// Sets num2 to zero
    pub fn multiply<'a, 'b, 'c, A, B, C>(
        &mut self,
        num1: &mut MutableByteRef<'a, A>,
        num2: &mut MutableByteRef<'b, B>,
        output: &mut MutableByteRef<'c, C>,
    ) where
        MutableByteRef<'a, A>: MarkSet,
        MutableByteRef<'b, B>: MarkSet,
        MutableByteRef<'c, C>: MarkSet,
    {
        self.loop_over_cell(num2.pointer, |ctx| {
            // subtract one from num2 because we are looping over it
            ctx.add_to_var(
                num2,
                Signedu8 {
                    negative: true,
                    value: 1,
                },
            );
            let temp_range = ctx.reserve(1);
            // temp var to store copy of num1
            let temp = temp_range.start;
            ctx.move_cell(num1.pointer, temp);
            // loop over our copy of num1, adding 1 to the output and putting the temp variable back into num1 for efficiency
            ctx.loop_over_cell(temp, |ctx| {
                ctx.point(temp);
                ctx.write_code("-");
                ctx.add_to_var(num1, Signedu8::from(1));
                ctx.add_to_var(output, Signedu8::from(1));
            });
            ctx.free_optional(temp_range);
        })
    }
    /// Multiplies num1 by a number known at compile time
    /// If destroy_num1 is false, then the contents of num1 will be preserved at the cost of more code being generatedk
    pub fn multiply_const<'a, 'b, A, B>(
        &mut self,
        num1: &mut MutableByteRef<'a, A>,
        num2: u8,
        output: &mut MutableByteRef<'b, B>,
        destroy_num1: bool,
    ) where
        MutableByteRef<'a, A>: MarkSet,
        MutableByteRef<'b, B>: MarkSet,
    {
        if destroy_num1 {
            self.loop_over_cell(num1.pointer, |ctx| {
                ctx.in_place_add_cell(
                    num1.pointer,
                    Signedu8 {
                        value: 1,
                        negative: true,
                    },
                );
                ctx.in_place_add(output, Signedu8::from(num2));
            })
        } else {
            let temp = self.reserve(1).start;
            self.move_cell(num1.pointer, temp);
            self.loop_over_cell(temp, |ctx| {
                ctx.in_place_add_cell(
                    temp,
                    Signedu8 {
                        value: 1,
                        negative: true,
                    },
                );
                ctx.in_place_add(output, Signedu8::from(num2));
                ctx.in_place_add(num1, Signedu8::from(1));
            })
        }
    }
    // cool division algorithm that daniel cristofani gave to me: [>+>-[>>>]<[[>+<-]>>+>]<<<<-]
    // "(Dividend remainder divisor quotient zero zero). This is easy to adapt for other memory layouts,
    // just the distance from one zero to the other needs to be the same as the distance from divisor to remainder or dividend, or some other known nonzero."
    pub fn divide(
        &mut self,
        dividend: Variable<ByteData>,
        divisor: Variable<ByteData>,
    ) -> DivisionResult {
        let division_space = self.declare_array(5);
        let dividend_temp = division_space.pointer.start;
        let divisor_temp = division_space.pointer.start + 2;
        self.move_cell(dividend.pointer.start + 1, dividend_temp);
        self.move_cell(divisor.pointer.start + 1, divisor_temp);
        self.point(division_space.pointer.start);
        self.write_code("[>+>-[>>>]<[[>+<-]>>+>]<<<<-]");
        let mut remainder = self.declare_byte();
        let mut quotient = self.declare_byte();
        self.move_cell(
            division_space.pointer.start + 1,
            remainder.pointer.start + 1,
        );
        remainder.var_data.has_been_set = true;
        self.move_cell(division_space.pointer.start + 3, quotient.pointer.start + 1);
        quotient.var_data.has_been_set = true;
        self.free_optional(division_space);
        DivisionResult {
            remainder,
            quotient,
        }
    }
    pub fn declare_and_set_byte(&mut self, value: u8) -> Variable<ByteData> {
        let mut var = self.declare_byte();
        self.set_var(&mut var.get_byte_ref(), value);
        var
    }
    pub fn null_terminated_array_iter(
        &mut self,
        array: Variable<ArrayData>,
        for_each: impl FnOnce(&mut BfContext),
    ) {
        self.point(array.pointer.start);
        self.write_code("[-]");
        self.point_add(1);
        self.write_code("[");
        for_each(self);
        self.point(array.pointer.start + 1);
        self.write_code(">]");
        self.write_code("<[<]");
        self.pointer = array.pointer.start;
    }
    pub fn pow<'a, 'b, 'c, A, B, C>(
        &mut self,
        base: &mut ByteRef<'a, A>,
        exponent: &mut MutableByteRef<'b, B>,
        output: &mut MutableByteRef<'c, C>,
    ) where
        MutableByteRef<'a, A>: MarkSet,
        MutableByteRef<'b, B>: MarkSet,
        MutableByteRef<'c, C>: MarkSet,
    {
        self.point(output.pointer);
        self.write_code("+");
        self.loop_over_cell(exponent.pointer, |ctx| {
            ctx.add_to_var(
                exponent,
                Signedu8 {
                    negative: true,
                    value: 1,
                },
            );
            let mut temp_output = ctx.declare_byte();
            let mut cloned_base = ctx.declare_byte();
            ctx.clone_cell(
                base.pointer,
                cloned_base.pointer.start + 1,
                cloned_base.pointer.start,
            );
            ctx.multiply(
                output,
                &mut cloned_base.get_byte_ref(),
                &mut temp_output.get_byte_ref(),
            );
            ctx.point(output.pointer);
            ctx.write_code("[-]");
            ctx.move_byte(&mut temp_output.get_byte_ref(), output);
        });
    }
    pub fn in_place_add<'a, T>(&mut self, byte_to_set: &mut MutableByteRef<'a, T>, to_add: Signedu8)
    where
        MutableByteRef<'a, T>: MarkSet,
    {
        self.in_place_add_cell(byte_to_set.pointer, to_add);
        byte_to_set.mark_set();
    }
    fn in_place_add_cell(&mut self, cell: usize, to_add: Signedu8) {
        self.point(cell);
        let to_repeat = if to_add.negative { "-" } else { "+" };
        self.write_code(&to_repeat.repeat(to_add.value.into()));
    }
    pub fn display_byte_as_decimal(&mut self,byte: Variable<ByteData>){
        let hundred=self.declare_and_set_byte(100);
        let mut result=self.divide(byte, hundred);
        let mut result_cloned=self.clone_var(&result.quotient);
        let mut first_was_zero=self.declare_byte();
        self.do_if_nonzero_mut(result_cloned.get_byte_ref(), |ctx|{
            ctx.add_to_var(&mut result.quotient.get_byte_ref(), Signedu8::from(48));
            ctx.point(result.quotient.get_byte_ref().pointer);
            ctx.write_code(".");
            ctx.in_place_add(&mut first_was_zero.get_byte_ref(), Signedu8::from(1));
        });
        let ten=self.declare_and_set_byte(10);
        let mut tens_result=self.divide(result.remainder, ten);
        let mut tens_result_cloned=self.clone_var(&tens_result.quotient);
        self.add_to_var(&mut tens_result.quotient.get_byte_ref(), Signedu8::from(48));
        self.do_if_nonzero_mut(tens_result_cloned.get_byte_ref(),|ctx|{
            ctx.in_place_add(&mut first_was_zero.get_byte_ref(), Signedu8::from(1));
        });
        self.do_if_nonzero_mut(first_was_zero.get_byte_ref(), |ctx|{
            ctx.point(tens_result.quotient.get_byte_ref().pointer);
            ctx.write_code(".");
        });
        self.point(tens_result.remainder.get_byte_ref().pointer);
        self.add_to_var(&mut tens_result.remainder.get_byte_ref(), Signedu8::from(48));
        self.write_code(".");
    }
}
pub trait GetRange {
    fn get_range(&self) -> MemoryRange;
}
impl<T> GetRange for Variable<T> {
    fn get_range(&self) -> MemoryRange {
        self.pointer
    }
}
impl GetRange for MemoryRange {
    fn get_range(&self) -> MemoryRange {
        *self
    }
}

pub struct DivisionResult {
    pub quotient: Variable<ByteData>,
    pub remainder: Variable<ByteData>,
}
pub struct MatchBuilder<'a> {
    ctx: &'a mut BfContext,
    var: Variable<ByteData>,
    codes: HashMap<u8, String>,
}

impl<'a> MatchBuilder<'a> {
    pub fn case(mut self, num: u8, to_do: impl FnOnce(&mut BfContext)) -> Self {
        let old = self.ctx.code.clone();
        self.ctx.do_if_zero(&self.var, to_do);
        let chars_added = self.ctx.code.len() - old.len();
        let diff = self.ctx.code[self.ctx.code.len() - chars_added..].to_owned();
        self.ctx.code = old;
        self.ctx.pointer = self.var.pointer.start;
        self.codes.insert(num, diff);
        self
    }
    pub fn build(mut self) {
        let mut sorted = self.codes.into_iter().collect::<Vec<(u8, String)>>();
        sorted.sort_by(|(num1, _), (num2, _)| num2.cmp(num1));
        let mut subtracted = 0;
        for (num, code) in sorted {
            let to_subtract = num.abs_diff(subtracted);
            println!("{to_subtract}");
            self.ctx.add_to_var(
                &mut self.var.get_byte_ref(),
                Signedu8 {
                    negative: true,
                    value: to_subtract,
                },
            );
            self.ctx.point(self.var.pointer.start);
            self.ctx.write_code(&code);
            subtracted = num;
        }
    }
}
impl<T> Pointable for ByteRef<'_, T> {
    fn get_pointer(&self) -> usize {
        self.pointer
    }
}
pub struct ByteRef<'a, T> {
    data_index: usize,
    pointer: usize,
    var: &'a mut Variable<T>,
}
/// Alias for ByteRef to hint that the function taking the MutableByteRef may mutate your data
pub type MutableByteRef<'a, T> = ByteRef<'a, T>;
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
pub trait Pointable {
    fn get_pointer(&self) -> usize;
}
impl Pointable for usize {
    fn get_pointer(&self) -> usize {
        *self
    }
}
impl<T> Pointable for &Variable<T> {
    fn get_pointer(&self) -> usize {
        self.pointer.start
    }
}
#[derive(Clone, Copy)]
pub struct MemoryRange {
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
    pub value: u8,
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
impl From<Signedu8> for i16 {
    fn from(val: Signedu8) -> Self {
        let sign: i16 = val.signum().into();
        let value: i16 = val.value.into();
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
    fn add_value(value: u8,negative:bool) {
        let mut ctx = BfContext::default();
        let mut testing = ctx.declare_byte();
        let mut byte_ref = testing.get_byte_ref();
        let pointer = byte_ref.pointer;
        ctx.add_to_var(&mut byte_ref, Signedu8{value,negative});
        let code = ctx.code;
        let mut run = interpreter::BfInterpreter::new_with_code(code);
        run.run(&mut BlankIO, &mut BlankIO).unwrap();
        let mut should_be=value;
        if negative{
            should_be=0_u8.wrapping_sub(value);
        }
        assert_eq!(run.cells[pointer], should_be);
    }
    #[test]
    fn add() {
        for i in 0..=255 {
            add_value(i,false);
            add_value(i,true)
        }
    }
    #[test]
    fn subtract() {
        for i in 0..=255 {
            let mut ctx = BfContext::default();
            let mut testing = ctx.declare_byte();
            ctx.point(testing.pointer.start + 1);
            ctx.write_code("-");
            ctx.add_to_var(
                &mut testing.get_byte_ref(),
                Signedu8 {
                    negative: true,
                    value: i,
                },
            );
            println!("{i}={}", &ctx.code);
            let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
            run.run(&mut BlankIO, &mut BlankIO).unwrap();
            assert_eq!(run.cells[testing.pointer.start + 1], 255 - i)
        }
    }
    #[test]
    fn print() {
        let mut ctx = BfContext::default();
        let test_str = "The quick brown fox jumps over the lazy DOG1234567890";
        ctx.display_text(test_str);
        let mut writer: Vec<u8> = Vec::new();
        let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
        run.run(&mut writer, &mut BlankIO).unwrap();
        assert_eq!(writer, test_str.as_bytes())
    }
    #[test]
    fn equals() {
        let mut ctx = BfContext::default();
        let value = 2;
        let var1 = ctx.declare_and_set_byte(3);
        let var2 = ctx.declare_and_set_byte(3);
        let mut is_good = ctx.declare_byte();
        let mut byte_ref = is_good.get_byte_ref();
        let pointer = byte_ref.pointer;
        ctx.do_if_equal(&var1, &var2, |ctx| {
            ctx.add_to_var(&mut byte_ref, Signedu8::from(value));
        });
        println!("{}", &ctx.code);
        let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
        run.run(&mut BlankIO, &mut BlankIO).unwrap();
        println!("{pointer}");
        println!("{:?}", &run.cells[..20]);
        assert_eq!(value, run.cells[pointer])
    }
    #[test]
    fn move_cell() {
        let mut ctx = BfContext::default();
        let value = 6;
        let var1 = ctx.declare_and_set_byte(value);
        let var2 = ctx.declare_byte();
        ctx.move_cell(var1.pointer.start + 1, var2.pointer.start + 1);
        let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
        run.run(&mut BlankIO, &mut BlankIO).unwrap();
        assert_eq!(run.cells[var1.pointer.start + 1], 0);
        assert_eq!(run.cells[var2.pointer.start + 1], value);
    }
    #[test]
    fn left_greater_than_right() {
        let test_values = [
            (5, 10),
            (6, 4),
            (10, 10),
            (255, 255),
            (0, 0),
            (1, 0),
            (0, 1),
            (3, 255),
            (255, 3),
        ];
        let value = 39;
        for test_value in test_values {
            let mut ctx = BfContext::default();
            let left = ctx.declare_and_set_byte(test_value.0);
            let right = ctx.declare_and_set_byte(test_value.1);
            let mut should_be_set = ctx.declare_byte();
            ctx.do_if_left_greater_than_right(left, right, |ctx| {
                ctx.set_var(&mut should_be_set.get_byte_ref(), value);
            });
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
    #[test]
    fn set_array() {
        for i in 1..10_u8 {
            let mut ctx = BfContext::default();
            let mut testing_array = ctx.declare_array(i.into());
            let test_values: Vec<u8> = (1..=i).collect();
            ctx.set_array(&test_values, &mut testing_array);
            let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
            run.run(&mut BlankIO, &mut BlankIO).unwrap();
            assert_eq!(
                run.cells
                    [testing_array.pointer.start + 1..testing_array.pointer.start + 1 + i as usize],
                test_values
            );
        }
    }
    #[test]
    fn do_if_zero() {
        let mut ctx: BfContext = BfContext::default();
        let zero = ctx.declare_byte();
        let mut should_be_set = ctx.declare_byte();
        let value = 92;
        ctx.do_if_zero(&zero, |ctx| {
            ctx.set_var(&mut should_be_set.get_byte_ref(), value);
        });
        let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
        run.run(&mut BlankIO, &mut BlankIO).unwrap();
        assert_eq!(run.cells[should_be_set.pointer.start + 1], value);
    }
    #[test]
    fn match_num() {
        let to_set_to = 123;
        let test_values: &[(&[u8], usize)] = &[
            (&[3, 4, 1, 5, 6, 255], 5),
            (&[243], 0),
            (&[0], 0),
            (&[5], 0),
        ];
        for test_value in test_values {
            let mut ctx: BfContext = BfContext::default();
            let correct = test_value.0[test_value.1];
            let to_set = ctx.declare_and_set_byte(correct);
            let mut should_set = ctx.declare_byte();
            let mut matching = ctx.match_num(to_set);
            for num in test_value.0 {
                if *num == correct {
                    matching = matching.case(*num, |ctx| {
                        ctx.add_to_var(&mut should_set.get_byte_ref(), to_set_to.into());
                    });
                } else {
                    matching = matching.case(*num, |_ctx| {});
                }
            }
            matching.build();
            println!("{test_value:?}");
            println!("{}\n\n", &ctx.code);
            let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
            run.run(&mut BlankIO, &mut BlankIO).unwrap();
            assert_eq!(run.cells[should_set.pointer.start + 1], to_set_to);
        }
    }
    #[test]
    fn divide() {
        for dividend in 1..12 {
            for divisor in 1..12 {
                let mut ctx: BfContext = BfContext::default();
                let dividend_var = ctx.declare_and_set_byte(dividend);
                let divisor_var = ctx.declare_and_set_byte(divisor);
                let answer = ctx.divide(dividend_var, divisor_var);
                let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
                run.run(&mut BlankIO, &mut BlankIO).unwrap();
                let quotient_correct = dividend / divisor;
                let remainder_correct = dividend % divisor;
                assert_eq!(
                    run.cells[answer.quotient.pointer.start + 1],
                    quotient_correct
                );
                assert_eq!(
                    run.cells[answer.remainder.pointer.start + 1],
                    remainder_correct
                )
            }
        }
    }
    #[test]
    fn multiply() {
        for first in 0..8 {
            for second in 0..(255 / first.max(1)) {
                let mut ctx = BfContext::default();
                let mut first_var = ctx.declare_and_set_byte(first);
                let mut second_var = ctx.declare_and_set_byte(second);
                let mut output = ctx.declare_byte();
                ctx.multiply(
                    &mut first_var.get_byte_ref(),
                    &mut second_var.get_byte_ref(),
                    &mut output.get_byte_ref(),
                );
                let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
                run.run(&mut BlankIO, &mut BlankIO).unwrap();
                assert_eq!(run.cells[output.pointer.start + 1], first * second);
            }
        }
    }
    #[test]
    fn multiply_const() {
        fn inner(destroy: bool) {
            for first in 0..8 {
                for second in 0..(255 / first.max(1)) {
                    let mut ctx = BfContext::default();
                    let mut first_var = ctx.declare_and_set_byte(first);
                    let mut output = ctx.declare_byte();
                    ctx.multiply_const(
                        &mut first_var.get_byte_ref(),
                        second,
                        &mut output.get_byte_ref(),
                        destroy,
                    );
                    let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
                    run.run(&mut BlankIO, &mut BlankIO).unwrap();
                    assert_eq!(run.cells[output.pointer.start + 1], first * second);
                }
            }
        }
        inner(true);
        inner(false);
    }
    #[test]
    fn pow() {
        for base in 1_u8..=255 {
            let floored = 256.0_f64.log(base as f64).floor() as u8;
            for pow in 1..floored {
                let mut ctx = BfContext::default();
                let mut base_var = ctx.declare_and_set_byte(base);
                let mut pow_var = ctx.declare_and_set_byte(pow);
                let mut output = ctx.declare_byte();
                ctx.pow(
                    &mut base_var.get_byte_ref(),
                    &mut pow_var.get_byte_ref(),
                    &mut output.get_byte_ref(),
                );
                println!("{}", ctx.code);
                let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
                run.run(&mut BlankIO, &mut BlankIO).unwrap();
                println!("{base}^{pow}");
                assert_eq!(run.cells[output.pointer.start + 1], base.pow(pow.into()));
            }
        }
    }
    #[test]
    fn display_byte_as_decimal(){
        for num in 0..=255_u8{
            let mut ctx=BfContext::default();
            let var=ctx.declare_and_set_byte(num);
            ctx.display_byte_as_decimal(var);
            println!("{}",ctx.code);
            let mut run = interpreter::BfInterpreter::new_with_code(ctx.code);
            let mut out: Vec<u8> = Vec::new();
            run.run(&mut out,&mut BlankIO).unwrap();
            assert_eq!(out,format!("{num}").as_bytes());
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
