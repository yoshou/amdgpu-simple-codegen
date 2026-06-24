pub struct CallingConvValueAssign {

}

pub struct CallingConvState {
    arg_locs: Vec<CallingConvValueAssign>,
}

impl CallingConvState {
    pub fn new() -> Self {
        CallingConvState {
            arg_locs: vec![]
        }
    }
}