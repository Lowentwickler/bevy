use crate::pipeline::AsVertexBufferDescriptor;
use bevy_core::Byteable;

#[repr(C)]
#[derive(Clone, Copy, AsVertexBufferDescriptor)]
#[as_crate(bevy_render)]
pub struct Vertex {
    pub position: [f32; 3],
    pub normal: [f32; 3],
    pub uv: [f32; 2],
    pub joints: [f32; 4],
    pub weights: [f32; 4],
}

// SAFE: Vertex is repr(C) containing primitives
unsafe impl Byteable for Vertex {}
