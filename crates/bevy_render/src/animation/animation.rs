use bevy_math::Mat4;

#[derive(Debug)]
pub struct Animation {
    pub name: String,
    pub keyframe: Option<Vec<f32>>,
    pub transform: Option<Vec<Vec<Mat4>>>,
}

impl Animation {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            keyframe: None,
            transform: None,
        }
    }
}
