use thiserror::Error;

const CELLS_LEN: usize = 30000;
pub struct BfInterpreter {
    pub cells: [u8; CELLS_LEN],
    pub memory_pointer: usize,
    pub code_pointer: usize,
    pub code: Vec<BfInstruction>,
}

impl BfInterpreter {
    pub fn new_with_code(code: String) -> Self {
        let mut parsed = Vec::new();
        for char in code.chars() {
            let parsed_char = match char {
                '+' => BfInstruction::Add,
                '-' => BfInstruction::Subtract,
                '>' => BfInstruction::PointRight,
                '<' => BfInstruction::PointLeft,
                '[' => BfInstruction::StartLoop,
                ']' => BfInstruction::EndLoop,
                ','=>BfInstruction::Input,
                '.'=>BfInstruction::Output,
                _ => continue,
            };
            parsed.push(parsed_char)
        }
        Self {
            cells: [0; CELLS_LEN],
            memory_pointer: 0,
            code_pointer: 0,
            code: parsed,
        }
    }
    pub fn run(
        &mut self,
        output: &mut impl std::io::Write,
        input: &mut impl std::io::Read,
    ) -> Result<(), BfError> {
        let mut loop_starts: Vec<usize> = Vec::new();
        while let Some(instruction) = self.code.get(self.code_pointer) {
            let current_cell = self
                .cells
                .get_mut(self.memory_pointer)
                .ok_or(BfError::CellPointerOutOfRange)?;
            match instruction {
                BfInstruction::Add => *current_cell=current_cell.wrapping_add(1),
                BfInstruction::Subtract => *current_cell=current_cell.wrapping_sub(1),
                BfInstruction::PointRight => self.memory_pointer += 1,
                BfInstruction::PointLeft => {
                    self.memory_pointer=self.memory_pointer.checked_sub(1).ok_or(BfError::CellPointerOutOfRange)?;
                },
                BfInstruction::StartLoop => loop_starts.push(self.code_pointer),
                BfInstruction::EndLoop => {
                    if *current_cell==0{
                        loop_starts.pop();
                    }else{
                        match loop_starts.last(){
                            Some(pointer)=>self.code_pointer=*pointer,
                            None=>return Err(BfError::MismatchedBracketsError)
                        }
                    }

                }
                BfInstruction::Output => {
                    output.write(&[*current_cell]).map_err(BfError::IoError)?;
                },
                BfInstruction::Input=>{
                    let mut to_read_to=[0];
                    input.read_exact(&mut to_read_to).map_err(BfError::IoError)?;
                }
            }
            self.code_pointer += 1
        }
        Ok(())
    }
}
#[derive(Error, Debug)]
pub enum BfError {
    #[error("error reading or writing byte")]
    IoError(std::io::Error),
    #[error("cell pointer out of range")]
    CellPointerOutOfRange,
    #[error("no matching close bracket found for opening bracket")]
    MismatchedBracketsError,
}
pub enum BfInstruction {
    Add,
    Subtract,
    PointRight,
    PointLeft,
    StartLoop,
    EndLoop,
    Output,
    Input,
}
