"""Encode existing native renders and verify the exported glTF payload."""
from __future__ import annotations

from itertools import chain
from pathlib import Path
import json
import math
import struct
import sys
from PIL import Image


def animation(folder: Path,destination: Path) -> dict:
    frames=tuple(map(lambda p:Image.open(p).convert('RGB'),sorted(folder.glob('frame-*.png'))))
    frames[0].save(destination,save_all=True,append_images=frames[1:],duration=83,loop=0,quality=90,method=6)
    return {'file':destination.name,'frames':len(frames),'width':frames[0].width,'height':frames[0].height,'fps':12}


def glb_report(path: Path) -> dict:
    payload=path.read_bytes()
    n,kind=struct.unpack_from('<II',payload,12)
    document=json.loads(payload[20:20+n])
    binary=payload[28+n:]
    component={5126:('f',4),5123:('H',2),5121:('B',1),5125:('I',4)}
    arity={'SCALAR':1,'VEC2':2,'VEC3':3,'VEC4':4,'MAT4':16}
    def accessor(index: int) -> tuple[tuple[float|int,...],...]:
        a=document['accessors'][index]; view=document['bufferViews'][a['bufferView']]
        fmt,size=component[a['componentType']]; count=arity[a['type']]
        offset=view.get('byteOffset',0)+a.get('byteOffset',0)
        stride=view.get('byteStride',size*count)
        return tuple(map(lambda i:struct.unpack_from('<'+fmt*count,binary,offset+stride*i),range(a['count'])))
    weights=tuple(chain.from_iterable(accessor(p['attributes']['WEIGHTS_0']) for mesh in document['meshes'] for p in mesh['primitives']))
    clips=tuple({'name':a['name'],'duration_seconds':max(accessor(s['input'])[-1][0] for s in a['samplers']),
                 'channels':len(a['channels'])} for a in document.get('animations',[]))
    report={'meshes':len(document['meshes']),'skins':len(document.get('skins',[])),
            'joints':tuple(document['nodes'][i]['name'] for i in document['skins'][0]['joints']),
            'animations':clips,'max_weight_sum_error':max(abs(sum(w)-1) for w in weights),
            'all_weights_finite':all(map(math.isfinite,chain.from_iterable(weights)))}
    return {**report,'passed':report['skins']==1 and report['meshes']==18 and len(clips)==2
            and report['max_weight_sum_error']<1e-6 and report['all_weights_finite']}


def contact_sheet(root: Path) -> None:
    rows=tuple(tuple(Image.open(root/name/f'frame-{i:04d}.png').convert('RGB').resize((260,312)) for i in indices)
               for name,indices in (('idle',(1,19,37,55)),('walk',(1,9,17,25))))
    canvas=Image.new('RGB',(1040,624),'#060e19')
    tuple(map(lambda row:tuple(map(lambda cell:canvas.paste(cell[1],(cell[0]*260,row[0]*312)),enumerate(row[1]))),enumerate(rows)))
    canvas.save(root/'motion-contact-sheet.jpg',quality=92)


def main(root: Path) -> None:
    report=glb_report(root/'knight-rig.glb')
    (root/'export-checks.json').write_text(json.dumps(report,indent=2)+'\n')
    if not report['passed']:
        raise SystemExit('Exported skeleton/clip/weight checks failed')
    previews=tuple(animation(root/clip,root/f'knight-{clip}.webp') for clip in ('idle','walk'))
    contact_sheet(root)
    (root/'previews.json').write_text(json.dumps(previews,indent=2)+'\n')
    print(json.dumps({'export':report,'previews':previews},indent=2))


if __name__=='__main__':
    main(Path(sys.argv[1]))
