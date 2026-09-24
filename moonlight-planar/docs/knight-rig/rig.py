"""Native Planar layers -> editable 2.5D Knight fan-art rig.

The closed skeleton, binding field and sampled motion are pure descriptions.
Blender is only the mesh/constraint/keyframe/render/export interpreter.
"""
from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from itertools import chain
from math import cos, sin, pi, isfinite
from pathlib import Path
import json
import sys

import bmesh
import bpy
from mathutils import Vector

Vec3 = tuple[float, float, float]


class Bone(Enum):
    ROOT = "ROOT"
    HIPS = "Hips"
    CHEST = "Chest"
    HEAD = "Head"
    THIGH_L = "Thigh.L"
    SHIN_L = "Shin.L"
    FOOT_L = "Foot.L"
    THIGH_R = "Thigh.R"
    SHIN_R = "Shin.R"
    FOOT_R = "Foot.R"
    ARM_L = "UpperArm.L"
    FOREARM_L = "Forearm.L"
    ARM_R = "UpperArm.R"
    FOREARM_R = "Forearm.R"
    IK_L = "CTRL_Foot.L"
    IK_R = "CTRL_Foot.R"
    POLE_L = "CTRL_Knee.L"
    POLE_R = "CTRL_Knee.R"
    CAPE_0 = "Cloak.Outer.L"
    HEM_0 = "Hem.Outer.L"
    CAPE_1 = "Cloak.Inner.L"
    HEM_1 = "Hem.Inner.L"
    CAPE_2 = "Cloak.Center"
    HEM_2 = "Hem.Center"
    CAPE_3 = "Cloak.Inner.R"
    HEM_3 = "Hem.Inner.R"
    CAPE_4 = "Cloak.Outer.R"
    HEM_4 = "Hem.Outer.R"


class Finish(Enum):
    SHELL = "Porcelain | warm ivory"
    VOID = "Void | velvet black"
    CLOTH = "Cloak | blue graphite"
    FOLD = "Cloak | silver fold"
    EYE = "Eyes | absolute dark"


class Clip(Enum):
    IDLE = "Idle"
    WALK = "Walk"

    @property
    def frames(self) -> int:
        return 72 if self is Clip.IDLE else 32


@dataclass(frozen=True)
class BoneSpec:
    bone: Bone
    head: Vec3
    tail: Vec3
    parent: Bone | None
    deform: bool = True


@dataclass(frozen=True)
class Rigid:
    bone: Bone


@dataclass(frozen=True)
class Joint:
    upper: Bone
    lower: Bone
    z: float
    radius: float


Skin = Rigid | Joint


@dataclass(frozen=True)
class Layer:
    name: str
    depth: float
    finish: Finish
    skin: Skin
    tint: float = 1.0


@dataclass(frozen=True)
class Transform:
    bone: Bone
    location: Vec3 = (0.0, 0.0, 0.0)
    roll: float = 0.0


CAPE_PAIRS = ((Bone.CAPE_0, Bone.HEM_0), (Bone.CAPE_1, Bone.HEM_1),
              (Bone.CAPE_2, Bone.HEM_2), (Bone.CAPE_3, Bone.HEM_3), (Bone.CAPE_4, Bone.HEM_4))
LEG_SIDES = ((-0.115, 0.015, Bone.THIGH_L, Bone.SHIN_L, Bone.FOOT_L, Bone.IK_L, Bone.POLE_L),
             (0.115, -0.025, Bone.THIGH_R, Bone.SHIN_R, Bone.FOOT_R, Bone.IK_R, Bone.POLE_R))


def leg_bones(side: tuple[float, float, Bone, Bone, Bone, Bone, Bone]) -> tuple[BoneSpec, ...]:
    x, y, thigh, shin, foot, ik, pole = side
    hip, knee, ankle = (x,y,0.57), (x+0.022,y,0.305), (x,y,0.075)
    return (BoneSpec(thigh,hip,knee,Bone.HIPS), BoneSpec(shin,knee,ankle,thigh),
            BoneSpec(foot,ankle,(x+0.11,y,0.075),shin),
            BoneSpec(ik,ankle,(x+0.11,y,0.075),Bone.ROOT,False),
            BoneSpec(pole,(x+0.6,y,0.3),(x+0.6,y,0.4),Bone.ROOT,False))


def cloak_bones(index: int, bones: tuple[Bone, Bone]) -> tuple[BoneSpec, ...]:
    axis = index-2
    root, hem = bones
    return (BoneSpec(root,(axis*0.064,-0.11,1.025),(axis*0.12,-0.11,0.74),Bone.CHEST),
            BoneSpec(hem,(axis*0.12,-0.11,0.74),(axis*0.166,-0.11,0.46),root))


SKELETON = (
    BoneSpec(Bone.ROOT,(0,0,0),(0,0,0.18),None,False),
    BoneSpec(Bone.HIPS,(0,0,0.57),(0,0,0.74),Bone.ROOT),
    BoneSpec(Bone.CHEST,(0,0,0.74),(0,0,1.025),Bone.HIPS),
    BoneSpec(Bone.HEAD,(0,0,1.025),(0,0,1.50),Bone.CHEST),
    BoneSpec(Bone.ARM_L,(-0.23,0.005,0.935),(-0.208,0.005,0.745),Bone.CHEST),
    BoneSpec(Bone.FOREARM_L,(-0.208,0.005,0.745),(-0.23,0.005,0.57),Bone.ARM_L),
    BoneSpec(Bone.ARM_R,(0.23,-0.04,0.935),(0.252,-0.04,0.745),Bone.CHEST),
    BoneSpec(Bone.FOREARM_R,(0.252,-0.04,0.745),(0.23,-0.04,0.57),Bone.ARM_R),
) + tuple(chain.from_iterable(map(leg_bones,LEG_SIDES))) + tuple(chain.from_iterable(
    map(lambda pair: cloak_bones(*pair),enumerate(CAPE_PAIRS))))

LAYERS = (
    Layer("Mask",-0.15,Finish.SHELL,Rigid(Bone.HEAD)),
    Layer("Eye_L",-0.207,Finish.EYE,Rigid(Bone.HEAD)),
    Layer("Eye_R",-0.207,Finish.EYE,Rigid(Bone.HEAD)),
    Layer("Body",0.045,Finish.VOID,Joint(Bone.CHEST,Bone.HIPS,0.73,0.10)),
    Layer("Collar",-0.15,Finish.FOLD,Rigid(Bone.CHEST)),
    Layer("Leg_L",0.015,Finish.VOID,Joint(Bone.THIGH_L,Bone.SHIN_L,0.305,0.07)),
    Layer("Leg_R",-0.025,Finish.VOID,Joint(Bone.THIGH_R,Bone.SHIN_R,0.305,0.07)),
    Layer("Foot_L",0.015,Finish.VOID,Rigid(Bone.FOOT_L)),
    Layer("Foot_R",-0.025,Finish.VOID,Rigid(Bone.FOOT_R)),
    Layer("Arm_L",0.005,Finish.VOID,Joint(Bone.ARM_L,Bone.FOREARM_L,0.745,0.065)),
    Layer("Arm_R",-0.04,Finish.VOID,Joint(Bone.ARM_R,Bone.FOREARM_R,0.745,0.065)),
    Layer("Hand_L",0.005,Finish.VOID,Rigid(Bone.FOREARM_L)),
    Layer("Hand_R",-0.04,Finish.VOID,Rigid(Bone.FOREARM_R)),
) + tuple(map(lambda pair: Layer(f"Cloak_{pair[0]}",-0.10-0.006*(2-abs(pair[0]-2)),
    Finish.CLOTH,Joint(pair[1][0],pair[1][1],0.74,0.15), (0.78,1.10,0.95,1.18,0.82)[pair[0]]),enumerate(CAPE_PAIRS)))


def smooth(t: float) -> float:
    return t*t*(3-2*t)


def weights(skin: Skin,z: float) -> tuple[tuple[Bone,float], ...]:
    match skin:
        case Rigid(bone): return ((bone,1.0),)
        case Joint(upper,lower,center,radius):
            high = smooth(max(0.0,min(1.0,(z-center+radius)/(2*radius))))
            return tuple(filter(lambda pair: pair[1]>0,((upper,high),(lower,1-high))))


DUTY = 0.62
STRIDE = 0.29
TRAVEL = STRIDE / DUTY


def foot_path(phase: float) -> tuple[float,float]:
    p = phase % 1.0
    if p < DUTY:
        return (STRIDE*(0.5-p/DUTY),0.0)
    u = (p-DUTY)/(1-DUTY)
    return (STRIDE*(-0.5+smooth(u)),0.092*sin(pi*u)**2)


def motion(clip: Clip,t: float) -> tuple[Transform,...]:
    phase = 2*pi*t
    idle = clip is Clip.IDLE
    root_x = 0 if idle else TRAVEL*(t-0.5)
    bob = -0.022+0.004*sin(phase) if idle else -0.044+0.007*cos(2*phase)
    sway = 0.018*sin(phase) if idle else 0.025*sin(phase)
    feet = ((0.0,0.0),(0.0,0.0)) if idle else (foot_path(t),foot_path(t+0.5))
    # Hips local Y points upward. Foot-control local Y points forward, Z upward.
    return (
        Transform(Bone.ROOT,(root_x,0,0)),
        Transform(Bone.HIPS,(0,bob,0),sway),
        Transform(Bone.CHEST,roll=-0.65*sway),
        Transform(Bone.HEAD,roll=(-0.018*sin(phase-0.5) if idle else -0.028*sin(phase-0.45))),
        Transform(Bone.IK_L,(0,feet[0][0],feet[0][1])),
        Transform(Bone.IK_R,(0,feet[1][0],feet[1][1])),
        Transform(Bone.ARM_L,roll=0.07+(0.02 if idle else 0.20)*sin(phase+pi)),
        Transform(Bone.ARM_R,roll=-0.07+(0.02 if idle else 0.20)*sin(phase)),
        Transform(Bone.FOREARM_L,roll=-0.12-0.025*sin(phase)),
        Transform(Bone.FOREARM_R,roll=0.10+0.025*sin(phase)),
    ) + tuple(chain.from_iterable(map(lambda pair: (
        Transform(pair[1][0],roll=(0.010 if idle else 0.033)*sin(phase-0.35+pair[0]*0.25)),
        Transform(pair[1][1],roll=(0.025 if idle else 0.080)*sin(phase-1.0+pair[0]*0.30)),
    ),enumerate(CAPE_PAIRS))))


# Blender interpreter: geometry is imported from the one native mesher.
def clean_mesh(mesh: bpy.types.Mesh) -> None:
    bm = bmesh.new()
    bm.from_mesh(mesh)
    bmesh.ops.remove_doubles(bm,verts=tuple(bm.verts),dist=1e-7)
    bmesh.ops.recalc_face_normals(bm,faces=tuple(bm.faces))
    bm.to_mesh(mesh)
    bm.free()
    mesh.update()


def material(finish: Finish,tint: float=1.0) -> bpy.types.Material:
    colors = {Finish.SHELL:(0.77,0.80,0.76,1), Finish.VOID:(0.006,0.009,0.017,1),
              Finish.CLOTH:(0.07,0.095,0.14,1), Finish.FOLD:(0.13,0.17,0.23,1), Finish.EYE:(0.001,0.002,0.003,1)}
    mat = bpy.data.materials.new(f"{finish.value} | {tint:.2f}")
    mat.diffuse_color = tuple(map(lambda c:c*tint,colors[finish][:3]))+(1.0,)
    mat.use_nodes=True
    bsdf=mat.node_tree.nodes.get("Principled BSDF")
    bsdf.inputs['Base Color'].default_value=mat.diffuse_color
    bsdf.inputs['Roughness'].default_value=0.8 if finish is not Finish.SHELL else 0.56
    bsdf.inputs['Specular IOR Level'].default_value=0.18 if finish is Finish.EYE else 0.28
    return mat


def prepare_layer(layer: Layer) -> bpy.types.Object:
    obj=bpy.data.objects[layer.name]
    clean_mesh(obj.data)
    # Bake layer depth into the mesh so all bind transforms share one coordinate system.
    obj.data.vertices.foreach_set('co',tuple(chain.from_iterable(map(
        lambda v:(v.co.x,v.co.y+layer.depth,v.co.z),obj.data.vertices))))
    obj.data.materials.append(material(layer.finish,layer.tint))
    bevel=obj.modifiers.new("Soft cut edge","BEVEL")
    bevel.width=0.005 if layer.finish is Finish.SHELL else 0.0015
    bevel.segments=3
    # Bevel before skinning: never smooth or move a rigged boundary after binding.
    bpy.context.view_layer.objects.active=obj
    bpy.ops.object.modifier_apply(modifier=bevel.name)
    return obj


def edit_bone(spec: BoneSpec,armature: bpy.types.Armature) -> None:
    bone=armature.edit_bones.new(spec.bone.value)
    bone.head,bone.tail=spec.head,spec.tail
    bone.parent=armature.edit_bones[spec.parent.value] if spec.parent else None
    bone.use_deform=spec.deform


def constrain_leg(rig: bpy.types.Object,side: tuple[float,float,Bone,Bone,Bone,Bone,Bone]) -> None:
    _,_,_,shin,foot,target,pole=side
    ik=rig.pose.bones[shin.value].constraints.new('IK')
    ik.name="Planted foot | two fixed-length links"
    ik.target,ik.subtarget=rig,target.value
    ik.pole_target,ik.pole_subtarget=rig,pole.value
    ik.chain_count,ik.use_stretch=2,False
    ik.pole_angle=0
    copy=rig.pose.bones[foot.value].constraints.new('COPY_ROTATION')
    copy.name="Sole orientation"
    copy.target,copy.subtarget=rig,target.value
    copy.target_space,copy.owner_space='WORLD','WORLD'


def create_rig() -> bpy.types.Object:
    data=bpy.data.armatures.new("Knight | articulated skeleton")
    rig=bpy.data.objects.new("KNIGHT | 2.5D rig",data)
    bpy.context.scene.collection.objects.link(rig)
    bpy.context.view_layer.objects.active=rig
    rig.select_set(True)
    bpy.ops.object.mode_set(mode='EDIT')
    tuple(map(lambda spec:edit_bone(spec,data),SKELETON))
    bpy.ops.object.mode_set(mode='OBJECT')
    rig.show_in_front=True
    data.display_type='OCTAHEDRAL'
    tuple(map(lambda bone:setattr(bone,'rotation_mode','XYZ'),rig.pose.bones))
    tuple(map(lambda bone:setattr(bone,'lock_scale',(True,True,True)),rig.pose.bones))
    tuple(map(lambda side:constrain_leg(rig,side),LEG_SIDES))
    controls=data.collections.new('Controls | root, feet, knees')
    deform=data.collections.new('Deform | head, limbs, cloak')
    tuple(map(lambda spec:(controls if not spec.deform else deform).assign(data.bones[spec.bone.value]),SKELETON))
    tuple(map(lambda spec:setattr(data.bones[spec.bone.value].color,'palette','THEME04' if not spec.deform else 'THEME03'),SKELETON))
    return rig


def bind(obj: bpy.types.Object,layer: Layer,rig: bpy.types.Object) -> None:
    influences=tuple(map(lambda v:weights(layer.skin,v.co.z),obj.data.vertices))
    bones=tuple(dict.fromkeys(map(lambda pair:pair[0],chain.from_iterable(influences))))
    groups={bone:obj.vertex_groups.new(name=bone.value) for bone in bones}
    tuple(map(lambda pair:tuple(map(lambda weight:groups[weight[0]].add((pair[0],),weight[1],'REPLACE'),pair[1])),enumerate(influences)))
    mod=obj.modifiers.new("Skeleton | normalized analytic weights",'ARMATURE')
    mod.object=rig
    mod.use_deform_preserve_volume=False
    obj.parent=rig


def apply_pose(rig: bpy.types.Object,clip: Clip,t: float,key: int|None=None) -> None:
    def apply(transform: Transform) -> None:
        bone=rig.pose.bones[transform.bone.value]
        bone.location=transform.location
        bone.rotation_euler=(0,0,transform.roll)
        if key is not None:
            bone.keyframe_insert(data_path='location',frame=key,group=bone.name)
            bone.keyframe_insert(data_path='rotation_euler',frame=key,group=bone.name)
    tuple(map(apply,motion(clip,t)))
    rig.update_tag()
    bpy.context.view_layer.update()


def make_action(rig: bpy.types.Object,clip: Clip) -> bpy.types.Action:
    rig.animation_data_create()
    rig.animation_data.action=None
    tuple(map(lambda i:apply_pose(rig,clip,i/clip.frames,i+1),range(clip.frames+1)))
    action=rig.animation_data.action
    action.name=f"Knight | {clip.value}"
    action.use_fake_user=True
    return action


def evaluated_points(obj: bpy.types.Object) -> tuple[Vec3,...]:
    result=obj.evaluated_get(bpy.context.evaluated_depsgraph_get())
    return tuple(map(lambda v:tuple(v.co),result.data.vertices))


def area(points: tuple[Vec3,...],indices: tuple[int,int,int]) -> float:
    a,b,c=tuple(map(lambda i:Vector(points[i]),indices))
    return (b-a).cross(c-a).length*0.5


def validate(rig: bpy.types.Object,objects: tuple[bpy.types.Object,...]) -> dict:
    def mesh_report(obj: bpy.types.Object) -> dict:
        obj.data.calc_loop_triangles()
        return {'name':obj.name,'vertices':len(obj.data.vertices),
                'max_weight_error':max(map(lambda v:abs(sum(map(lambda w:w.weight,v.groups))-1),obj.data.vertices)),
                'influences':max(map(lambda v:len(v.groups),obj.data.vertices))}
    def pose_report(clip: Clip,t: float) -> dict:
        apply_pose(rig,clip,t)
        point_sets=tuple(map(evaluated_points,objects))
        target_errors=tuple(map(lambda side:(rig.pose.bones[side[3].value].tail-rig.pose.bones[side[5].value].head).length,LEG_SIDES))
        expected_feet=((0.0,0.0),(0.0,0.0)) if clip is Clip.IDLE else (foot_path(t),foot_path(t+0.5))
        foot_path_errors=tuple(map(lambda pair:(rig.pose.bones[pair[0][5].value].head-Vector((
            pair[0][0]+pair[1][0]+(0 if clip is Clip.IDLE else TRAVEL*(t-0.5)),pair[0][1],0.075+pair[1][1]))).length,
            zip(LEG_SIDES,expected_feet)))
        return {'clip':clip.value,'phase':t,'finite':all(map(isfinite,chain.from_iterable(chain.from_iterable(point_sets)))),
                'minimum_area':min(chain.from_iterable(map(lambda pair:map(lambda tri:area(pair[1],tuple(tri.vertices)),pair[0].data.loop_triangles),zip(objects,point_sets)))),
                'max_foot_target_error':max(target_errors),
                'max_foot_path_error':max(foot_path_errors),
                'knees':tuple(map(lambda side:tuple(rig.pose.bones[side[3].value].head),LEG_SIDES))}
    meshes=tuple(map(mesh_report,objects))
    poses=tuple(pose_report(clip,i/16) for clip in Clip for i in range(17))
    return {'passed':all(map(lambda m:m['max_weight_error']<1e-6,meshes)) and all(map(
        lambda p:p['finite'] and p['minimum_area']>1e-12 and p['max_foot_target_error']<0.003 and p['max_foot_path_error']<1e-5,poses)),
        'meshes':meshes,'poses':poses,'scope':'34 sampled poses; foot IK reach and skin normalization, not all-pose collision certification.'}


def aim(obj: bpy.types.Object,target: Vec3) -> None:
    obj.rotation_euler=(Vector(target)-obj.location).to_track_quat('-Z','Y').to_euler()


def light(name: str,position: Vec3,color: Vec3,power: float,size: float) -> None:
    data=bpy.data.lights.new(name,'AREA')
    data.energy,data.color,data.shape,data.size=power,color,'DISK',size
    obj=bpy.data.objects.new(name,data)
    bpy.context.scene.collection.objects.link(obj)
    obj.location=position
    aim(obj,(0,0,0.9))


def stage(rig: bpy.types.Object) -> None:
    scene=bpy.context.scene
    scene.render.engine='CYCLES'
    scene.cycles.samples=20
    scene.cycles.use_denoising=True
    scene.render.resolution_x,scene.render.resolution_y,scene.render.resolution_percentage=800,960,100
    scene.render.image_settings.file_format='PNG'
    scene.render.fps=24
    scene.world=bpy.data.worlds.new('Deep blue | quiet cavern')
    scene.world.use_nodes=True
    scene.world.node_tree.nodes['Background'].inputs['Color'].default_value=(0.025,0.045,0.09,1)
    scene.world.node_tree.nodes['Background'].inputs['Strength'].default_value=0.25
    scene.view_settings.view_transform='AgX'
    bpy.ops.mesh.primitive_plane_add(size=200,location=(0,0,0.017))
    floor=bpy.context.object
    floor.name='Stage | ground contact'
    ground=material(Finish.CLOTH,0.3)
    floor.data.materials.append(ground)
    camera_data=bpy.data.cameras.new('Portrait | front three-quarter')
    camera=bpy.data.objects.new('Portrait | front three-quarter',camera_data)
    scene.collection.objects.link(camera)
    camera.location=(0.45,-8,2.25)
    camera_data.type,camera_data.ortho_scale='ORTHO',2.27
    aim(camera,(0,0,0.93))
    follow=camera.constraints.new('COPY_LOCATION')
    follow.name='Preview follows accumulated root motion'
    follow.target,follow.subtarget=rig,Bone.ROOT.value
    follow.use_x,follow.use_y,follow.use_z,follow.use_offset=True,False,False,True
    scene.camera=camera
    tuple(map(lambda args:light(*args),(
        ('Ivory softbox',(-2,-4,4),(0.78,0.87,1),240,3),
        ('Moon rim',(1,1,3),(0.25,0.58,1),330,2),
        ('Warm bounce',(3,-2,1),(1,0.77,0.56),70,2),
    )))


GUIDE = '''KNIGHT | 2.5D FAN-ART RIG

Select KNIGHT | 2.5D rig and enter Pose Mode.
ROOT places the character. Hips and Chest control weight/lean; Head remains rigid.
CTRL_Foot.L / .R move the feet; CTRL_Knee.L / .R choose knee direction.
UpperArm and Forearm are FK controls. Cloak and Hem bones shape each cloth panel.
Clear the active action to pose manually; the feet remain IK-controlled.

Action Editor: Knight | Idle (frames 1-73, 3 seconds), Knight | Walk (1-33, 1.333 seconds).
The endpoint repeats the local pose; display frames 1-72 or 1-32 for looping playback.
Walk carries +X root motion of 0.468 m per cycle. Accumulate that translation when
repeating the action to retain planted world-space feet. Idle is stationary.
The walk is authored, not motion capture; cloak motion is keyed, not cloth simulation.

Native mesh layers were triangulated once by Moonlight Planar constrained Delaunay.
These are shallow solid layers for front and modest three-quarter views, not a
fully modeled all-angle character. The dark eyes are rigid mesh inlays.
GLB contains skinning and sampled Idle/Walk; editable IK constraints stay in Blender.

Fan art of the Knight from Hollow Knight. Original character: Team Cherry.
https://www.hollowknight.com/ . No game asset or texture was extracted.
'''


def export(rig: bpy.types.Object,objects: tuple[bpy.types.Object,...],actions: tuple[bpy.types.Action,...],output: Path) -> None:
    scene=bpy.context.scene
    rig.animation_data.action=actions[0]
    scene.frame_start,scene.frame_end=1,Clip.IDLE.frames
    scene.frame_set(1)
    bpy.ops.object.select_all(action='DESELECT')
    tuple(map(lambda o:o.select_set(True),(rig,)+objects))
    bpy.context.view_layer.objects.active=rig
    bpy.ops.export_scene.gltf(filepath=str(output/'knight-rig.glb'),export_format='GLB',use_selection=True,
        export_animations=True,export_animation_mode='ACTIONS',export_force_sampling=True,
        export_bake_animation=True,export_anim_slide_to_zero=True,
        export_skins=True,export_def_bones=True,export_extras=True)
    bpy.data.texts.new('START HERE | Knight controls').write(GUIDE)
    (output/'README.txt').write_text(GUIDE)
    bpy.ops.object.select_all(action='DESELECT')
    rig.select_set(True)
    bpy.context.view_layer.objects.active=rig
    bpy.ops.wm.save_as_mainfile(filepath=str(output/'knight-rig.blend'))
    scene.render.filepath=str(output/'knight-rig.png')
    bpy.ops.render.render(write_still=True)


def render_clip(rig: bpy.types.Object,action: bpy.types.Action,clip: Clip,output: Path) -> None:
    scene=bpy.context.scene
    rig.animation_data.action=action
    scene.frame_start,scene.frame_end,scene.frame_step=1,clip.frames,2
    scene.render.resolution_percentage=65
    scene.cycles.samples=12
    folder=output/clip.value.lower()
    folder.mkdir(exist_ok=True)
    scene.render.filepath=str(folder/'frame-')
    bpy.ops.render.render(animation=True)


def render_skeleton(rig: bpy.types.Object,action: bpy.types.Action,output: Path) -> None:
    """X-ray diagram derived from evaluated bones, not a separately authored rig."""
    rig.animation_data.action=action
    scene=bpy.context.scene
    scene.frame_set(9)
    scene.render.resolution_percentage=100
    glow=bpy.data.materials.new('Diagram | joint cyan')
    glow.diffuse_color=(0.10,0.85,0.80,1)
    glow.use_nodes=True
    node=glow.node_tree.nodes.get('Principled BSDF')
    node.inputs['Base Color'].default_value=glow.diffuse_color
    node.inputs['Emission Color'].default_value=glow.diffuse_color
    node.inputs['Emission Strength'].default_value=0.5
    def segment(spec: BoneSpec) -> bpy.types.Object:
        bone=rig.pose.bones[spec.bone.value]
        data=bpy.data.curves.new('Diagram | '+spec.bone.value,'CURVE')
        data.dimensions,data.bevel_depth,data.bevel_resolution='3D',0.006,3
        spline=data.splines.new('POLY')
        spline.points.add(1)
        spline.points.foreach_set('co',(bone.head.x,-0.42,bone.head.z,1,bone.tail.x,-0.42,bone.tail.z,1))
        obj=bpy.data.objects.new(data.name,data)
        scene.collection.objects.link(obj)
        data.materials.append(glow)
        return obj
    segments=tuple(map(segment,filter(lambda spec:spec.deform,SKELETON)))
    scene.render.filepath=str(output/'knight-skeleton.png')
    bpy.ops.render.render(write_still=True)
    tuple(map(lambda obj:bpy.data.objects.remove(obj,do_unlink=True),segments))


def main() -> None:
    args=tuple(sys.argv[sys.argv.index('--')+1:]) if '--' in sys.argv else ()
    if len(args)!=2:
        raise SystemExit('usage: blender -b --factory-startup --python rig.py -- INPUT.obj OUTPUT_DIRECTORY')
    source,output=map(Path,args)
    output.mkdir(parents=True,exist_ok=True)
    bpy.ops.wm.read_factory_settings(use_empty=True)
    bpy.ops.wm.obj_import(filepath=str(source),forward_axis='Y',up_axis='Z')
    objects=tuple(map(prepare_layer,LAYERS))
    bpy.ops.object.select_all(action='DESELECT')
    rig=create_rig()
    tuple(map(lambda pair:bind(pair[0],pair[1],rig),zip(objects,LAYERS)))
    report=validate(rig,objects)
    (output/'rig-checks.json').write_text(json.dumps(report,indent=2))
    print(json.dumps({'passed':report['passed'],'poses':report['poses']}),flush=True)
    if not report['passed']:
        raise SystemExit('Rig admission failed; inspect rig-checks.json')
    actions=tuple(map(lambda clip:make_action(rig,clip),Clip))
    stage(rig)
    export(rig,objects,actions,output)
    tuple(map(lambda pair:render_clip(rig,pair[0],pair[1],output),zip(actions,Clip)))
    render_skeleton(rig,actions[1],output)


if __name__=='__main__':
    main()
