use bevy::{
    prelude::*,
    render::{
        mesh::shape,
        animation::Animation,
        pipeline::{DynamicBinding, PipelineDescriptor, PipelineSpecialization, RenderPipeline},
        render_graph::{base, AssetRenderResourcesNode, RenderResourcesNode, RenderGraph},
        renderer::RenderResources,
        shader::{ShaderStage, ShaderStages},
    },
    gltf::gltf_reader::{
        GltfAssets,
        load_gltf,
    },
};

use fs::File;
use io::Read;
use std::{
    fs, io,
    path::{Path},
};

fn main() {
    App::build()
        .add_resource(Msaa { samples: 4 })
        .add_default_plugins()
        .add_asset::<MyMaterial>()
        .add_startup_system(setup.system())
        .add_system(animate.system())
        .run();
}

/// Just to load all at once avoiding AssetServer
fn load_scene(asset_path: &str) -> Result<GltfAssets, io::Error> {
    let path = Path::new(asset_path);
    let mut file = File::open(path)?;
    let mut bytes = Vec::new();
    file.read_to_end(&mut bytes)?;
    let result = load_gltf(path, bytes).unwrap();
    Ok(result)
}

#[derive(RenderResources, Default)]
struct MyMaterial {
    pub color: Color,
}

#[derive(RenderResources, Default)]
struct JointTransforms {
    joint_count: u32,
    #[render_resources(buffer)]
    joint_transforms: Vec<Mat4>,
}

const VERTEX_SHADER: &str = r#"
#version 450
layout(location = 0) in vec3 Vertex_Position;
layout(location = 1) in vec3 Vertex_Normal;
layout(location = 2) in vec2 Vertex_Uv;
layout(location = 3) in vec4 Vertex_Joints;
layout(location = 4) in vec4 Vertex_Weights;

layout(location = 0) out vec3 v_Position;
layout(location = 1) out vec3 v_Normal;
layout(location = 2) out vec2 v_Uv;
layout(set = 0, binding = 0) uniform Camera {
    mat4 ViewProj;
};
layout(set = 1, binding = 0) uniform Transform {
    mat4 Model;
};
layout(set = 1, binding = 1) uniform MyMaterial_color {
    vec4 color;
};
layout(set = 1, binding = 2) uniform JointTransforms_joint_count {
    uint joint_count;
};
layout(set = 1, binding = 3) uniform JointTransforms_joint_transforms {
    mat4 joint_transforms[24];
};
void main() {
    v_Normal = mat3(Model) * Vertex_Normal;
    v_Position = (Model * vec4(Vertex_Position, 1.0)).xyz;
    v_Uv = Vertex_Uv;
    gl_Position = ViewProj * vec4(v_Position, 1.0);
}
"#;

const FRAGMENT_SHADER: &str = r#"
#version 450

const int MAX_LIGHTS = 10;

struct Light {
    mat4 proj;
    vec4 pos;
    vec4 color;
};

layout(location = 0) in vec3 v_Position;
layout(location = 1) in vec3 v_Normal;
layout(location = 2) in vec2 v_Uv;

layout(location = 0) out vec4 o_Target;
layout(set = 1, binding = 1) uniform MyMaterial_color {
    vec4 color;
};

layout(set = 2, binding = 0) uniform Lights {
    uvec4 NumLights;
    Light SceneLights[MAX_LIGHTS];
};
void main() {
    vec4 output_color = color;
    vec3 normal = normalize(v_Normal);
    vec3 ambient = vec3(0.05, 0.05, 0.05);
    // accumulate color
    vec3 light_color = ambient;
    for (int i=0; i<int(NumLights.x) && i<MAX_LIGHTS; ++i) {
        Light light = SceneLights[i];
        // compute Lambertian diffuse term
        vec3 light_dir = normalize(light.pos.xyz - v_Position);
        float diffuse = max(0.0, dot(normal, light_dir));
        // add light contribution
        light_color += diffuse * light.color.xyz;
    }
    output_color.xyz *= light_color;
    o_Target = output_color;
}
"#;


fn setup(
    mut commands: Commands,
    mut pipelines: ResMut<Assets<PipelineDescriptor>>,
    mut shaders: ResMut<Assets<Shader>>,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<MyMaterial>>,
    mut render_graph: ResMut<RenderGraph>,
) {
    // load the scene from GLTF
    let mut scene = load_scene("assets/models/Fox.gltf").unwrap();

    // Create a new shader pipeline
    let pipeline_handle = pipelines.add(PipelineDescriptor::default_config(ShaderStages {
        vertex: shaders.add(Shader::from_glsl(ShaderStage::Vertex, VERTEX_SHADER)),
        fragment: Some(shaders.add(Shader::from_glsl(ShaderStage::Fragment, FRAGMENT_SHADER))),
    }));

    // Add an AssetRenderResourcesNode to our Render Graph. This will bind MyMaterial resources to our shader
    render_graph.add_system_node(
        "my_material",
        AssetRenderResourcesNode::<MyMaterial>::new(true),
    );


    // Add a Render Graph edge connecting our new "my_material" node to the main pass node. This ensures "my_material" runs before the main pass
    render_graph
        .add_node_edge("my_material", base::node::MAIN_PASS)
        .unwrap();

    render_graph.add_system_node("joint_transforms", RenderResourcesNode::<JointTransforms>::new(true)); // false indiciates that this is not a dynamic uniform
    render_graph.add_node_edge("joint_transforms", base::node::MAIN_PASS).unwrap();

    // Create a new material
    let material = materials.add(MyMaterial {
        color: Color::rgb(0.8, 0.0, 0.0),
    });

    let mut animation = scene.animation.pop().unwrap();
    // TODO: set actual data here
    let mut jointTransform = JointTransforms {
        joint_count: 24,
        joint_transforms: vec![Mat4::identity(); 24],
    };

    for mesh in scene.mesh {
        commands
        .spawn(MeshComponents {
            mesh: meshes.add(mesh),
            render_pipelines: RenderPipelines::from_pipelines(vec![RenderPipeline::specialized(
                pipeline_handle,
                // NOTE: in the future you wont need to manually declare dynamic bindings
                PipelineSpecialization {
                    dynamic_bindings: vec![
                        // Transform
                        DynamicBinding {
                            bind_group: 1,
                            binding: 0,
                        },
                        // MyMaterial_color
                        DynamicBinding {
                            bind_group: 1,
                            binding: 1,
                        },
                        // JointTransforms_joint_count
                        DynamicBinding {
                            bind_group: 1,
                            binding: 2,
                        },
                        // JointTransforms_joint_transforms
                        DynamicBinding {
                            bind_group: 1,
                            binding: 3,
                        },
                    ],
                    ..Default::default()
                },
            )]),
            translation: Translation::new(0.0, 0.0, 0.0),
            ..Default::default()
        })
        .with(animation)
        .with(jointTransform)
        .with(material);

        // handle only first entity
        break;
    }

    commands
        // light
        .spawn(LightComponents {
            translation: Translation::new(100.0, 150.0, 150.0),
            ..Default::default()
        })
        // camera
        .spawn(Camera3dComponents {
            transform: Transform::new_sync_disabled(Mat4::face_toward(
                Vec3::new(150.0, 100.0, 150.0),
                Vec3::new(0.0, 50.0, 0.0),
                Vec3::new(0.0, 1.0, 0.0),
            )),
            ..Default::default()
        });
}

fn animate(
    mut commands: Commands,
    time: Res<Time>,
    mut materials: ResMut<Assets<MyMaterial>>,
    mut query: Query<(&Animation, &JointTransforms)>
) {
    for (mut animation, mut joint_transform) in &mut query.iter() {
        if let Some(transform) = &animation.transform {
            println!("animation: {}, ({})", animation.name, transform.len());
        }
    }
}
