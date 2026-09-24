"""Native crescent OBJ -> solid, analytically weighted Blender sword.

Pure geometry/weight descriptions precede the Blender effect boundary.
Run: blender -b --factory-startup --python rig.py -- INPUT.obj OUTPUT_DIRECTORY
"""
from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from itertools import chain, product
from math import cos, floor, isfinite, pi, sin
from pathlib import Path
import json
import sys

import bmesh
import bpy
from mathutils import Vector


Vec3 = tuple[float, float, float]
Face = tuple[int, ...]
BLADE_LENGTH = 1.2
BONE_COUNT = 6
ROOT = "ROOT_Sword"


class Finish(Enum):
    STEEL = "Moonsteel"
    GOLD = "Pale gold"
    LEATHER = "Midnight leather"
    GLASS = "Moon glass"
    INLAY = "Ivory moon"


class Control(Enum):
    SIDE = "bend_side_deg"
    FLAT = "bend_flat_deg"
    TWIST = "twist_deg"


@dataclass(frozen=True)
class Drive:
    control: Control
    axis: int
    limit: float


DRIVES = (Drive(Control.SIDE, 2, 25.0), Drive(Control.FLAT, 0, 15.0), Drive(Control.TWIST, 1, 25.0))


@dataclass(frozen=True)
class Pose:
    name: str
    side: float = 0.0
    flat: float = 0.0
    twist: float = 0.0

    def value(self, control: Control) -> float:
        match control:
            case Control.SIDE: return self.side
            case Control.FLAT: return self.flat
            case Control.TWIST: return self.twist


@dataclass(frozen=True)
class MeshSpec:
    name: str
    vertices: tuple[Vec3, ...]
    faces: tuple[Face, ...]
    finish: Finish


@dataclass(frozen=True)
class Weight:
    bone: str
    value: float


def bone_name(index: int) -> str:
    return f"BLADE_{index + 1:02d}"


def skin_weights(z: float) -> tuple[Weight, ...]:
    """Adjacent-bone smoothstep partition of unity; the base is rigid."""
    u = max(0.0, min(BONE_COUNT - 1.0, z * BONE_COUNT / BLADE_LENGTH - 0.5))
    lower = floor(u)
    upper = min(BONE_COUNT - 1, lower + 1)
    t = u - lower
    mix = t * t * (3.0 - 2.0 * t)
    return (Weight(bone_name(lower), 1.0),) if lower == upper else tuple(filter(
        lambda weight: weight.value > 0.0,
        (Weight(bone_name(lower), 1.0 - mix), Weight(bone_name(upper), mix)),
    ))


def lathe(name: str, profile: tuple[tuple[float, float], ...], finish: Finish, sides: int = 32,
          flatten: float = 1.0) -> MeshSpec:
    vertices = tuple(map(lambda sample: (
        profile[sample[0]][1] * cos(2 * pi * sample[1] / sides),
        flatten * profile[sample[0]][1] * sin(2 * pi * sample[1] / sides),
        profile[sample[0]][0],
    ), product(range(len(profile)), range(sides))))
    walls = tuple(map(lambda cell: (
        cell[0] * sides + cell[1], cell[0] * sides + (cell[1] + 1) % sides,
        (cell[0] + 1) * sides + (cell[1] + 1) % sides, (cell[0] + 1) * sides + cell[1],
    ), product(range(len(profile) - 1), range(sides))))
    caps = (tuple(reversed(range(sides))), tuple(range((len(profile) - 1) * sides, len(profile) * sides)))
    return MeshSpec(name, vertices, walls + caps, finish)


def torus(name: str, center: Vec3, radius: float, thickness: float, finish: Finish) -> MeshSpec:
    major, minor = 64, 8
    vertices = tuple(map(lambda sample: (
        center[0] + (radius + thickness * cos(2 * pi * sample[1] / minor)) * cos(2 * pi * sample[0] / major),
        center[1] + thickness * sin(2 * pi * sample[1] / minor),
        center[2] + (radius + thickness * cos(2 * pi * sample[1] / minor)) * sin(2 * pi * sample[0] / major),
    ), product(range(major), range(minor))))
    faces = tuple(map(lambda cell: (
        cell[0] * minor + cell[1], ((cell[0] + 1) % major) * minor + cell[1],
        ((cell[0] + 1) % major) * minor + (cell[1] + 1) % minor,
        cell[0] * minor + (cell[1] + 1) % minor,
    ), product(range(major), range(minor))))
    return MeshSpec(name, vertices, faces, finish)


def diamond(name: str, width: float, height: float, front: float, finish: Finish) -> MeshSpec:
    return MeshSpec(name,
        ((0.0, -0.023, height), (width, -0.023, 0.0), (0.0, -0.023, -height),
         (-width, -0.023, 0.0), (0.0, front, 0.0), (0.0, 0.016, 0.0)),
        tuple(chain.from_iterable(map(lambda index: (
            (index, (index + 1) % 4, 4), ((index + 1) % 4, index, 5),
        ), range(4)))), finish)


def binding(index: int) -> MeshSpec:
    z = -0.068 - index * 0.0155
    return lathe(f"Grip binding {index + 1:02d}",
        ((z - 0.002, 0.028), (z, 0.0295), (z + 0.002, 0.028)), Finish.GOLD, flatten=0.78)


def pommel_disc() -> MeshSpec:
    disc = lathe("Pommel moon glass", ((-0.016, 0.042), (0.0, 0.046), (0.016, 0.042)), Finish.GLASS)
    return MeshSpec(disc.name, tuple(map(lambda v: (v[0], v[2], v[1] - 226 / 750), disc.vertices)), disc.faces, disc.finish)


def crescent_inlay() -> MeshSpec:
    """A shallow closed crescent ribbon, not a texture painted on the pommel."""
    steps = 40
    # Two meridian curves meet at their tips; interior cross-sections remain quads.
    rows = tuple(map(lambda index: index / steps, range(1, steps)))
    tip_top, tip_bottom = (0.015, -0.021, -0.269), (0.015, -0.021, -0.334)
    ribbon = tuple(chain.from_iterable(map(lambda t: (
        (0.015 - 0.047 * sin(pi * t), -0.021, -0.269 - 0.065 * t),
        (0.015 - 0.028 * sin(pi * t), -0.021, -0.269 - 0.065 * t),
    ), rows)))
    front = (tip_top,) + ribbon + (tip_bottom,)
    n = len(front)
    front_faces = ((0, 1, 2),) + tuple(map(lambda row: (
        1 + 2 * row, 3 + 2 * row, 4 + 2 * row, 2 + 2 * row,
    ), range(len(rows) - 1))) + ((n - 3, n - 1, n - 2),)
    boundary = (0,) + tuple(range(1, n - 1, 2)) + (n - 1,) + tuple(reversed(range(2, n - 1, 2)))
    back = tuple(map(lambda v: (v[0], v[1] + 0.002, v[2]), front))
    walls = tuple(map(lambda edge: (edge[0], edge[1], edge[1] + n, edge[0] + n),
        zip(boundary, boundary[1:] + boundary[:1])))
    return MeshSpec("Pommel crescent inlay", front + back,
        front_faces + tuple(map(lambda face: tuple(map(lambda i: i + n, reversed(face))), front_faces)) + walls,
        Finish.INLAY)


def fittings() -> tuple[MeshSpec, ...]:
    return (
        lathe("Leather grip", ((-0.27, 0.022), (-0.25, 0.027), (-0.06, 0.03), (-0.025, 0.032)), Finish.LEATHER, flatten=0.78),
        lathe("Grip lower ferrule", ((-0.271, 0.024), (-0.261, 0.03), (-0.252, 0.03)), Finish.GOLD, flatten=0.8),
        lathe("Grip upper ferrule", ((-0.056, 0.032), (-0.032, 0.034)), Finish.GOLD, flatten=0.8),
        diamond("Guard gemstone setting", 0.043, 0.072, -0.037, Finish.GOLD),
        diamond("Guard moon glass", 0.031, 0.053, -0.052, Finish.GLASS),
        pommel_disc(), torus("Pommel rim", (0.0, -0.012, -226 / 750), 0.045, 0.0028, Finish.GOLD),
        crescent_inlay(),
    ) + tuple(map(binding, range(12)))


POSE_TESTS = (
    Pose("neutral"), Pose("side positive", side=25), Pose("side negative", side=-25),
    Pose("flat positive", flat=15), Pose("flat negative", flat=-15),
    Pose("twist positive", twist=25), Pose("twist negative", twist=-25),
    Pose("combined positive", 25, 15, 25), Pose("combined negative", -25, -15, -25),
    Pose("above limit", 250, 150, 250),
)


# Blender effect boundary: construct data blocks, bind, evaluate, save and render.
def finish_material(finish: Finish, shade: int = 0) -> bpy.types.Material:
    material = bpy.data.materials.new(f"{finish.value} {shade:02d}")
    match finish:
        case Finish.STEEL: color, metal, rough = (0.10 + shade * 0.018, 0.27 + shade * 0.027, 0.38 + shade * 0.031, 1.0), 0.78, 0.3
        case Finish.GOLD: color, metal, rough = (0.48 + shade * 0.035, 0.36 + shade * 0.032, 0.16 + shade * 0.024, 1.0), 0.83, 0.3
        case Finish.LEATHER: color, metal, rough = (0.009, 0.018, 0.028, 1.0), 0.08, 0.68
        case Finish.GLASS: color, metal, rough = (0.028, 0.27, 0.40, 1.0), 0.55, 0.16
        case Finish.INLAY: color, metal, rough = (0.75, 0.84, 0.68, 1.0), 0.4, 0.27
    material.diffuse_color = color
    material.use_nodes = True
    shader = material.node_tree.nodes.get("Principled BSDF")
    shader.inputs["Base Color"].default_value = color
    shader.inputs["Metallic"].default_value = metal
    shader.inputs["Roughness"].default_value = rough
    return material


def clean_mesh(mesh: bpy.types.Mesh, weld: bool = False) -> None:
    editable = bmesh.new()
    editable.from_mesh(mesh)
    if weld:
        bmesh.ops.remove_doubles(editable, verts=tuple(editable.verts), dist=1e-8)
    bmesh.ops.recalc_face_normals(editable, faces=tuple(editable.faces))
    editable.to_mesh(mesh)
    editable.free()
    mesh.update()


def assign_materials(obj: bpy.types.Object, materials: tuple[bpy.types.Material, ...]) -> None:
    tuple(map(obj.data.materials.append, materials))
    obj.data.polygons.foreach_set("material_index", tuple(map(
        lambda face: int((0.5 + 0.5 * sin(face.index * 1.719)) * (len(materials) - 1)), obj.data.polygons)))


def add_uvs(mesh: bpy.types.Mesh) -> None:
    lower_x, upper_x = min(map(lambda v: v.co.x, mesh.vertices)), max(map(lambda v: v.co.x, mesh.vertices))
    lower_z, upper_z = min(map(lambda v: v.co.z, mesh.vertices)), max(map(lambda v: v.co.z, mesh.vertices))
    uv = mesh.uv_layers.new(name="Surface projection")
    values = tuple(chain.from_iterable(map(lambda loop: (
        (mesh.vertices[loop.vertex_index].co.x - lower_x) / max(1e-8, upper_x - lower_x),
        (mesh.vertices[loop.vertex_index].co.z - lower_z) / max(1e-8, upper_z - lower_z),
    ), mesh.loops)))
    uv.data.foreach_set("uv", values)


def create_mesh(spec: MeshSpec) -> bpy.types.Object:
    mesh = bpy.data.meshes.new(spec.name)
    mesh.from_pydata(spec.vertices, (), spec.faces)
    clean_mesh(mesh)
    obj = bpy.data.objects.new(spec.name, mesh)
    bpy.context.scene.collection.objects.link(obj)
    assign_materials(obj, (finish_material(spec.finish),))
    add_uvs(mesh)
    return obj


def import_native(path: Path) -> tuple[bpy.types.Object, bpy.types.Object]:
    bpy.ops.wm.obj_import(filepath=str(path), forward_axis="Y", up_axis="Z")
    blade, guard = bpy.data.objects["Blade"], bpy.data.objects["Guard"]
    tuple(map(lambda obj: clean_mesh(obj.data, weld=True), (blade, guard)))
    assign_materials(blade, tuple(map(lambda index: finish_material(Finish.STEEL, index), range(8))))
    assign_materials(guard, tuple(map(lambda index: finish_material(Finish.GOLD, index), range(6))))
    tuple(map(lambda obj: add_uvs(obj.data), (blade, guard)))
    return blade, guard


def edit_blade_bone(armature: bpy.types.Armature, index: int) -> None:
    bone = armature.edit_bones.new(bone_name(index))
    bone.head = (0.0, 0.0, index * BLADE_LENGTH / BONE_COUNT)
    bone.tail = (0.0, 0.0, (index + 1) * BLADE_LENGTH / BONE_COUNT)
    bone.parent = armature.edit_bones[ROOT if index == 0 else bone_name(index - 1)]
    bone.use_connect = True


def add_control(root: bpy.types.PoseBone, drive: Drive) -> None:
    root[drive.control.value] = 0.0
    root.id_properties_ui(drive.control.value).update(
        min=-drive.limit, max=drive.limit, soft_min=-drive.limit, soft_max=drive.limit,
        description=f"Total {drive.control.value.replace('_', ' ')}. Clamped; base, grip and guard stay rigid.")


def bone_driver(rig: bpy.types.Object, bone: bpy.types.PoseBone, drive: Drive) -> None:
    driver = bone.driver_add("rotation_euler", drive.axis).driver
    variable = driver.variables.new()
    variable.name = "angle"
    variable.type = "SINGLE_PROP"
    variable.targets[0].id = rig
    variable.targets[0].data_path = f'pose.bones["{ROOT}"]["{drive.control.value}"]'
    driver.expression = f"min(max(angle,{-drive.limit}),{drive.limit})*{pi / 180 / (BONE_COUNT - 1):.16f}"


def configure_blade_bone(rig: bpy.types.Object, index: int) -> None:
    bone = rig.pose.bones[bone_name(index)]
    bone.rotation_mode = "XYZ"
    bone.lock_location = (True, True, True)
    bone.lock_scale = (True, True, True)
    bone.lock_rotation = (True, True, True)
    bone.bone.color.palette = "THEME04"
    if index > 0:
        tuple(map(lambda drive: bone_driver(rig, bone, drive), DRIVES))


def create_rig() -> bpy.types.Object:
    armature = bpy.data.armatures.new("Vesper skeleton")
    rig = bpy.data.objects.new("VESPER | Rig", armature)
    bpy.context.scene.collection.objects.link(rig)
    bpy.context.view_layer.objects.active = rig
    rig.select_set(True)
    bpy.ops.object.mode_set(mode="EDIT")
    root = armature.edit_bones.new(ROOT)
    root.head, root.tail = (0.0, 0.0, -0.28), (0.0, 0.0, 0.0)
    tuple(map(lambda index: edit_blade_bone(armature, index), range(BONE_COUNT)))
    bpy.ops.object.mode_set(mode="OBJECT")
    rig.show_in_front = True
    armature.display_type = "BBONE"
    root_pose = rig.pose.bones[ROOT]
    root_pose.bone.color.palette = "THEME03"
    root_pose.lock_scale = (True, True, True)
    tuple(map(lambda drive: add_control(root_pose, drive), DRIVES))
    tuple(map(lambda index: configure_blade_bone(rig, index), range(BONE_COUNT)))
    return rig


def bind_mesh(obj: bpy.types.Object, rig: bpy.types.Object, deform: bool) -> None:
    names = tuple(map(bone_name, range(BONE_COUNT))) if deform else (ROOT,)
    groups = dict(map(lambda name: (name, obj.vertex_groups.new(name=name)), names))
    weights = tuple(map(lambda vertex: (vertex.index, skin_weights(vertex.co.z) if deform else (Weight(ROOT, 1.0),)), obj.data.vertices))
    tuple(map(lambda vertex: tuple(map(lambda weight: groups[weight.bone].add((vertex[0],), weight.value, "REPLACE"), vertex[1])), weights))
    modifier = obj.modifiers.new("Analytic skin | six bones", "ARMATURE")
    modifier.object = rig
    modifier.use_deform_preserve_volume = False
    modifier.use_bone_envelopes = False
    obj.parent = rig


def apply_pose(rig: bpy.types.Object, pose: Pose) -> None:
    root = rig.pose.bones[ROOT]
    tuple(map(lambda drive: root.__setitem__(drive.control.value, pose.value(drive.control)), DRIVES))
    rig.update_tag()
    bpy.context.view_layer.update()


def coordinates(obj: bpy.types.Object) -> tuple[Vec3, ...]:
    evaluated = obj.evaluated_get(bpy.context.evaluated_depsgraph_get())
    return tuple(map(lambda vertex: tuple(vertex.co), evaluated.data.vertices))


def topology_report(obj: bpy.types.Object) -> dict[str, object]:
    editable = bmesh.new()
    editable.from_mesh(obj.data)
    report = {"object": obj.name, "vertices": len(editable.verts), "faces": len(editable.faces),
              "nonmanifold_edges": sum(map(lambda edge: not edge.is_manifold, editable.edges)),
              "zero_area_faces": sum(map(lambda face: face.calc_area() <= 1e-16, editable.faces)),
              "max_weight_error": max(map(lambda vertex: abs(sum(map(lambda weight: weight.weight, vertex.groups)) - 1.0), obj.data.vertices))}
    editable.free()
    return report


def triangle_area(points: tuple[Vec3, ...], indices: tuple[int, int, int]) -> float:
    a, b, c = tuple(map(lambda index: Vector(points[index]), indices))
    return (b - a).cross(c - a).length * 0.5


def pose_report(rig: bpy.types.Object, blade: bpy.types.Object, rigid: tuple[bpy.types.Object, ...],
                rest: tuple[Vec3, ...], rigid_rest: tuple[tuple[Vec3, ...], ...], pose: Pose) -> dict[str, object]:
    apply_pose(rig, pose)
    posed = coordinates(blade)
    triangles = tuple(map(lambda triangle: tuple(triangle.vertices), blade.data.loop_triangles))
    ratios = tuple(map(lambda triangle: triangle_area(posed, triangle) / triangle_area(rest, triangle), triangles))
    drift = max(chain.from_iterable(map(lambda pair: map(
        lambda points: (Vector(points[0]) - Vector(points[1])).length, zip(coordinates(pair[0]), pair[1])), zip(rigid, rigid_rest))))
    return {"pose": pose.name, "finite": all(map(isfinite, chain.from_iterable(posed))),
            "min_face_area_ratio": min(ratios), "max_face_area_ratio": max(ratios), "rigid_drift_m": drift,
            "tip": posed[max(range(len(rest)), key=lambda index: rest[index][2])],
            "max_vertex_displacement_m": max(map(lambda pair: (Vector(pair[0]) - Vector(pair[1])).length, zip(rest, posed)))}


def validate_rig(rig: bpy.types.Object, blade: bpy.types.Object, rigid: tuple[bpy.types.Object, ...]) -> dict[str, object]:
    apply_pose(rig, Pose("neutral"))
    blade.data.calc_loop_triangles()
    rest, rigid_rest = coordinates(blade), tuple(map(coordinates, rigid))
    topology = tuple(map(topology_report, (blade,) + rigid))
    poses = tuple(map(lambda pose: pose_report(rig, blade, rigid, rest, rigid_rest, pose), POSE_TESTS))
    apply_pose(rig, Pose("neutral"))
    passed = all(map(lambda entry: entry["nonmanifold_edges"] == 0 and entry["zero_area_faces"] == 0 and entry["max_weight_error"] < 1e-6, topology)) and all(map(
        lambda entry: entry["finite"] and entry["min_face_area_ratio"] > 0.15 and entry["max_face_area_ratio"] < 5.0 and entry["rigid_drift_m"] < 1e-7, poses))
    clamped = (Vector(poses[-1]["tip"]) - Vector(poses[-3]["tip"])).length < 1e-7
    return {"passed": passed and clamped, "bounds_enforced": clamped, "topology": topology, "poses": poses,
            "scope": "Sampled bounded poses; not collision certification or physical metal simulation."}


def key_pose(rig: bpy.types.Object, frame: int, pose: Pose) -> None:
    apply_pose(rig, pose)
    tuple(map(lambda drive: rig.pose.bones[ROOT].keyframe_insert(data_path=f'["{drive.control.value}"]', frame=frame), DRIVES))


def animate_rig(rig: bpy.types.Object) -> None:
    tuple(map(lambda sample: key_pose(rig, sample[0], sample[1]), (
        (1, Pose("neutral")), (25, Pose("side", side=25)), (49, Pose("flat", flat=15)),
        (73, Pose("twist", twist=25)), (97, Pose("combined", -25, -15, -25)), (121, Pose("neutral")),
    )))
    rig.animation_data.action.name = "Vesper | bounded blade demonstration"
    bpy.context.scene.frame_start, bpy.context.scene.frame_end = 1, 120
    bpy.context.scene.render.fps = 24
    bpy.context.scene.frame_set(1)


def aim(obj: bpy.types.Object, target: Vec3) -> None:
    obj.rotation_euler = (Vector(target) - obj.location).to_track_quat("-Z", "Y").to_euler()


def area_light(name: str, location: Vec3, color: Vec3, energy: float, size: float) -> None:
    data = bpy.data.lights.new(name, "AREA")
    data.energy, data.color, data.shape, data.size = energy, color, "DISK", size
    obj = bpy.data.objects.new(name, data)
    bpy.context.scene.collection.objects.link(obj)
    obj.location = location
    aim(obj, (0.0, 0.0, 0.4))


def stage() -> None:
    scene = bpy.context.scene
    scene.render.engine = "CYCLES"
    scene.cycles.samples = 24
    scene.cycles.use_denoising = True
    scene.render.resolution_x, scene.render.resolution_y, scene.render.resolution_percentage = 840, 1080, 100
    scene.render.image_settings.file_format = "PNG"
    scene.world = bpy.data.worlds.new("Midnight studio")
    scene.world.use_nodes = True
    scene.world.node_tree.nodes["Background"].inputs["Color"].default_value = (0.065, 0.1, 0.18, 1.0)
    scene.world.node_tree.nodes["Background"].inputs["Strength"].default_value = 0.32
    camera_data = bpy.data.cameras.new("Vesper portrait")
    camera = bpy.data.objects.new("Vesper portrait", camera_data)
    scene.collection.objects.link(camera)
    camera.location = (0.95, -4.8, 1.55)
    camera_data.type, camera_data.ortho_scale = "ORTHO", 1.93
    aim(camera, (0.035, 0.0, 0.43))
    scene.camera = camera
    tuple(map(lambda spec: area_light(*spec), (
        ("Silver softbox", (-2.0, -3.0, 2.5), (0.64, 0.84, 1.0), 420.0, 3.0),
        ("Gold strip", (2.0, -1.5, 1.1), (1.0, 0.75, 0.43), 260.0, 1.4),
        ("Moon rim", (0.5, 2.0, 1.8), (0.23, 0.63, 1.0), 560.0, 2.0),
    )))
    floor_spec = MeshSpec("Studio floor", ((-100, -100, -0.39), (100, -100, -0.39), (100, 100, -0.39), (-100, 100, -0.39)), ((0, 1, 2, 3),), Finish.LEATHER)
    floor_obj = create_mesh(floor_spec)
    floor_obj.data.materials[0].node_tree.nodes.get("Principled BSDF").inputs["Roughness"].default_value = 0.42


GUIDE = """VESPER - RIGGED CRESCENT MOONBLADE

Select VESPER | Rig. In Pose Mode select ROOT_Sword.
Move/rotate ROOT_Sword to place the complete rigid sword.
N sidebar > Item > Properties (Custom Properties):
  bend_side_deg: -25 to +25 degrees
  bend_flat_deg: -15 to +15 degrees
  twist_deg:     -25 to +25 degrees
Zero all three for a rigid sword. Blender drivers clamp even scripted values.
The first blade segment and the grip, guard, gemstone and pommel stay rigid.
Six connected blade bones use analytic smoothstep weights, at most two per vertex.
No automatic weights, cloth solver, simulation cache or corrective shape keys.

Frames 1-120: a five-second sampled bend/twist demonstration. Frame 1 is neutral.
Clear the 'Vesper | bounded blade demonstration' action to pose without animation.
Native .blend keeps the controls. GLB retains skin/bones and baked animation;
Blender custom-property drivers are not portable glTF controls.

Solid native CDT blade and guard; fuller is an actual open tunnel. Material
facets are authored; projection UVs are provided, not a unique bake-ready atlas.
This is a stylized rigged prop, not a collision-certified or physical metal model.
"""


def save_asset(rig: bpy.types.Object, objects: tuple[bpy.types.Object, ...], output: Path) -> None:
    bpy.ops.object.select_all(action="DESELECT")
    tuple(map(lambda obj: obj.select_set(True), (rig,) + objects))
    bpy.context.view_layer.objects.active = rig
    bpy.ops.export_scene.gltf(filepath=str(output / "vesper-rig.glb"), export_format="GLB", use_selection=True,
        export_animations=True, export_animation_mode="SCENE", export_force_sampling=True,
        export_bake_animation=True, export_frame_range=True, export_def_bones=True,
        export_skins=True, export_materials="EXPORT", export_extras=True)
    bpy.ops.object.select_all(action="DESELECT")
    rig.select_set(True)
    bpy.context.view_layer.objects.active = rig
    bpy.ops.object.mode_set(mode="POSE")
    rig.data.bones.active = rig.data.bones[ROOT]
    rig.pose.bones[ROOT].select = True
    tuple(map(configure_viewport, chain.from_iterable(map(
        lambda screen: chain.from_iterable(map(lambda area: filter(lambda space: space.type == "VIEW_3D", area.spaces), screen.areas)),
        bpy.data.screens))))
    bpy.data.texts.new("START HERE - Vesper controls").write(GUIDE)
    bpy.ops.wm.save_as_mainfile(filepath=str(output / "vesper-rig.blend"))
    (output / "README.txt").write_text(GUIDE)


def configure_viewport(space: bpy.types.SpaceView3D) -> None:
    space.region_3d.view_perspective = "CAMERA"
    space.show_region_ui = True
    space.shading.type = "SOLID"
    space.shading.color_type = "MATERIAL"
    space.overlay.show_floor = False


def main() -> None:
    args = tuple(sys.argv[sys.argv.index("--") + 1:]) if "--" in sys.argv else ()
    if len(args) != 2:
        raise SystemExit("usage: blender -b --factory-startup --python rig.py -- INPUT.obj OUTPUT_DIRECTORY")
    source, output = Path(args[0]), Path(args[1])
    output.mkdir(parents=True, exist_ok=True)
    bpy.ops.wm.read_factory_settings(use_empty=True)
    blade, guard = import_native(source)
    ornaments = tuple(map(create_mesh, fittings()))
    rig = create_rig()
    bind_mesh(blade, rig, deform=True)
    tuple(map(lambda obj: bind_mesh(obj, rig, deform=False), (guard,) + ornaments))
    report = validate_rig(rig, blade, (guard,) + ornaments)
    (output / "deformation-checks.json").write_text(json.dumps(report, indent=2))
    print(json.dumps({"passed": report["passed"], "poses": report["poses"]}), flush=True)
    if not report["passed"]:
        raise SystemExit("Rig checks failed; see deformation-checks.json")
    animate_rig(rig)
    stage()
    save_asset(rig, (blade, guard) + ornaments, output)
    bpy.context.scene.render.filepath = str(output / "vesper-rig.png")
    bpy.ops.render.render(write_still=True)


if __name__ == "__main__":
    main()
