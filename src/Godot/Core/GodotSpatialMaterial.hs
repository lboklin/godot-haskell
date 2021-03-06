module Godot.Core.GodotSpatialMaterial where
import Data.Coerce
import Foreign.C
import Godot.Internal.Dispatch
import System.IO.Unsafe
import Godot.Gdnative.Internal
import Godot.Gdnative.Types
import Godot.Api.Auto

pattern DIFFUSE_BURLEY :: Int

pattern DIFFUSE_BURLEY = 0

pattern DEPTH_DRAW_ALWAYS :: Int

pattern DEPTH_DRAW_ALWAYS = 1

pattern FLAG_DISABLE_DEPTH_TEST :: Int

pattern FLAG_DISABLE_DEPTH_TEST = 2

pattern TEXTURE_MAX :: Int

pattern TEXTURE_MAX = 16

pattern DISTANCE_FADE_PIXEL_DITHER :: Int

pattern DISTANCE_FADE_PIXEL_DITHER = 2

pattern FLAG_DONT_RECEIVE_SHADOWS :: Int

pattern FLAG_DONT_RECEIVE_SHADOWS = 15

pattern TEXTURE_METALLIC :: Int

pattern TEXTURE_METALLIC = 1

pattern FEATURE_MAX :: Int

pattern FEATURE_MAX = 12

pattern TEXTURE_DETAIL_ALBEDO :: Int

pattern TEXTURE_DETAIL_ALBEDO = 14

pattern TEXTURE_CHANNEL_RED :: Int

pattern TEXTURE_CHANNEL_RED = 0

pattern SPECULAR_TOON :: Int

pattern SPECULAR_TOON = 3

pattern FEATURE_EMISSION :: Int

pattern FEATURE_EMISSION = 1

pattern TEXTURE_CHANNEL_BLUE :: Int

pattern TEXTURE_CHANNEL_BLUE = 2

pattern TEXTURE_AMBIENT_OCCLUSION :: Int

pattern TEXTURE_AMBIENT_OCCLUSION = 8

pattern TEXTURE_DETAIL_MASK :: Int

pattern TEXTURE_DETAIL_MASK = 13

pattern FEATURE_DETAIL :: Int

pattern FEATURE_DETAIL = 11

pattern SPECULAR_SCHLICK_GGX :: Int

pattern SPECULAR_SCHLICK_GGX = 0

pattern FEATURE_SUBSURACE_SCATTERING :: Int

pattern FEATURE_SUBSURACE_SCATTERING = 8

pattern FEATURE_DEPTH_MAPPING :: Int

pattern FEATURE_DEPTH_MAPPING = 7

pattern TEXTURE_REFRACTION :: Int

pattern TEXTURE_REFRACTION = 12

pattern FEATURE_TRANSPARENT :: Int

pattern FEATURE_TRANSPARENT = 0

pattern FEATURE_TRANSMISSION :: Int

pattern FEATURE_TRANSMISSION = 9

pattern FLAG_TRIPLANAR_USE_WORLD :: Int

pattern FLAG_TRIPLANAR_USE_WORLD = 10

pattern DEPTH_DRAW_ALPHA_OPAQUE_PREPASS :: Int

pattern DEPTH_DRAW_ALPHA_OPAQUE_PREPASS = 3

pattern FLAG_USE_ALPHA_SCISSOR :: Int

pattern FLAG_USE_ALPHA_SCISSOR = 13

pattern DIFFUSE_LAMBERT :: Int

pattern DIFFUSE_LAMBERT = 1

pattern DETAIL_UV_2 :: Int

pattern DETAIL_UV_2 = 1

pattern DIFFUSE_LAMBERT_WRAP :: Int

pattern DIFFUSE_LAMBERT_WRAP = 2

pattern BLEND_MODE_ADD :: Int

pattern BLEND_MODE_ADD = 1

pattern BILLBOARD_ENABLED :: Int

pattern BILLBOARD_ENABLED = 1

pattern TEXTURE_NORMAL :: Int

pattern TEXTURE_NORMAL = 4

pattern FEATURE_NORMAL_MAPPING :: Int

pattern FEATURE_NORMAL_MAPPING = 2

pattern TEXTURE_TRANSMISSION :: Int

pattern TEXTURE_TRANSMISSION = 11

pattern FLAG_ALBEDO_FROM_VERTEX_COLOR :: Int

pattern FLAG_ALBEDO_FROM_VERTEX_COLOR = 3

pattern FEATURE_REFRACTION :: Int

pattern FEATURE_REFRACTION = 10

pattern BLEND_MODE_MIX :: Int

pattern BLEND_MODE_MIX = 0

pattern BILLBOARD_FIXED_Y :: Int

pattern BILLBOARD_FIXED_Y = 2

pattern TEXTURE_FLOWMAP :: Int

pattern TEXTURE_FLOWMAP = 7

pattern TEXTURE_RIM :: Int

pattern TEXTURE_RIM = 5

pattern FEATURE_AMBIENT_OCCLUSION :: Int

pattern FEATURE_AMBIENT_OCCLUSION = 6

pattern DISTANCE_FADE_OBJECT_DITHER :: Int

pattern DISTANCE_FADE_OBJECT_DITHER = 3

pattern FLAG_BILLBOARD_KEEP_SCALE :: Int

pattern FLAG_BILLBOARD_KEEP_SCALE = 7

pattern DEPTH_DRAW_OPAQUE_ONLY :: Int

pattern DEPTH_DRAW_OPAQUE_ONLY = 0

pattern FLAG_USE_POINT_SIZE :: Int

pattern FLAG_USE_POINT_SIZE = 5

pattern CULL_DISABLED :: Int

pattern CULL_DISABLED = 2

pattern FEATURE_ANISOTROPY :: Int

pattern FEATURE_ANISOTROPY = 5

pattern TEXTURE_ROUGHNESS :: Int

pattern TEXTURE_ROUGHNESS = 2

pattern EMISSION_OP_ADD :: Int

pattern EMISSION_OP_ADD = 0

pattern FLAG_MAX :: Int

pattern FLAG_MAX = 18

pattern FLAG_FIXED_SIZE :: Int

pattern FLAG_FIXED_SIZE = 6

pattern FLAG_UNSHADED :: Int

pattern FLAG_UNSHADED = 0

pattern FLAG_ENSURE_CORRECT_NORMALS :: Int

pattern FLAG_ENSURE_CORRECT_NORMALS = 16

pattern TEXTURE_CHANNEL_GREEN :: Int

pattern TEXTURE_CHANNEL_GREEN = 1

pattern SPECULAR_BLINN :: Int

pattern SPECULAR_BLINN = 1

pattern CULL_BACK :: Int

pattern CULL_BACK = 0

pattern EMISSION_OP_MULTIPLY :: Int

pattern EMISSION_OP_MULTIPLY = 1

pattern FLAG_SRGB_VERTEX_COLOR :: Int

pattern FLAG_SRGB_VERTEX_COLOR = 4

pattern FLAG_EMISSION_ON_UV2 :: Int

pattern FLAG_EMISSION_ON_UV2 = 12

pattern FLAG_AO_ON_UV2 :: Int

pattern FLAG_AO_ON_UV2 = 11

pattern TEXTURE_DEPTH :: Int

pattern TEXTURE_DEPTH = 9

pattern DIFFUSE_OREN_NAYAR :: Int

pattern DIFFUSE_OREN_NAYAR = 3

pattern FLAG_UV1_USE_TRIPLANAR :: Int

pattern FLAG_UV1_USE_TRIPLANAR = 8

pattern DEPTH_DRAW_DISABLED :: Int

pattern DEPTH_DRAW_DISABLED = 2

pattern TEXTURE_DETAIL_NORMAL :: Int

pattern TEXTURE_DETAIL_NORMAL = 15

pattern FEATURE_RIM :: Int

pattern FEATURE_RIM = 3

pattern DISTANCE_FADE_DISABLED :: Int

pattern DISTANCE_FADE_DISABLED = 0

pattern SPECULAR_DISABLED :: Int

pattern SPECULAR_DISABLED = 4

pattern CULL_FRONT :: Int

pattern CULL_FRONT = 1

pattern TEXTURE_SUBSURFACE_SCATTERING :: Int

pattern TEXTURE_SUBSURFACE_SCATTERING = 10

pattern TEXTURE_CLEARCOAT :: Int

pattern TEXTURE_CLEARCOAT = 6

pattern TEXTURE_CHANNEL_ALPHA :: Int

pattern TEXTURE_CHANNEL_ALPHA = 3

pattern FEATURE_CLEARCOAT :: Int

pattern FEATURE_CLEARCOAT = 4

pattern FLAG_ALBEDO_TEXTURE_FORCE_SRGB :: Int

pattern FLAG_ALBEDO_TEXTURE_FORCE_SRGB = 14

pattern DIFFUSE_TOON :: Int

pattern DIFFUSE_TOON = 4

pattern DETAIL_UV_1 :: Int

pattern DETAIL_UV_1 = 0

pattern BILLBOARD_DISABLED :: Int

pattern BILLBOARD_DISABLED = 0

pattern BLEND_MODE_MUL :: Int

pattern BLEND_MODE_MUL = 3

pattern FLAG_USE_VERTEX_LIGHTING :: Int

pattern FLAG_USE_VERTEX_LIGHTING = 1

pattern FLAG_UV2_USE_TRIPLANAR :: Int

pattern FLAG_UV2_USE_TRIPLANAR = 9

pattern BLEND_MODE_SUB :: Int

pattern BLEND_MODE_SUB = 2

pattern SPECULAR_PHONG :: Int

pattern SPECULAR_PHONG = 2

pattern DISTANCE_FADE_PIXEL_ALPHA :: Int

pattern DISTANCE_FADE_PIXEL_ALPHA = 1

pattern FLAG_DISABLE_AMBIENT_LIGHT :: Int

pattern FLAG_DISABLE_AMBIENT_LIGHT = 17

pattern BILLBOARD_PARTICLES :: Int

pattern BILLBOARD_PARTICLES = 3

pattern TEXTURE_EMISSION :: Int

pattern TEXTURE_EMISSION = 3

pattern TEXTURE_CHANNEL_GRAYSCALE :: Int

pattern TEXTURE_CHANNEL_GRAYSCALE = 4

pattern TEXTURE_ALBEDO :: Int

pattern TEXTURE_ALBEDO = 0