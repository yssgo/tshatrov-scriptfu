Grue's GIMP scripts
===================

My Script-Fu scripts for GIMP.

AnimStack
---------
Animation tools package

* [GIMP Registry page](http://registry.gimp.org/node/26501)
* [Youtube tutorials](http://www.youtube.com/playlist?list=PLiASt6bCYQNCRlWbGrwyPnmJlwIzMziEq)
* [Tumblr](http://animstack.tumblr.com/)

BgMask
------
Masking/background extraction

* [GIMP Registry page](http://registry.gimp.org/node/27601)

Export Layers Plus
------------------
Export layers as individual images

* [GIMP Registry page](http://registry.gimp.org/node/28038)

LayerScript
-----------

### 설치

layerscript.scm 을 GIMP 2.10의 스크립트 폴더 중 하나에 저장.
- 김프 2.10의 사용자용 기본 스크립트 폴더
  - `%APPDATA%\GIMP\2.10\scripts`

### 설명

레이어 이름에 효과 지정 태그를 사용.

"Process LayerScript tags(레이어스크립트 태그 처리)" 메뉴를 실행하면,
레이어스크립트(LayerScript) 태그가 지정되고 링크(사슬 고리 표시)된 각 레이어에 대해,
효과 적용 레이어(레이어 이름이 LayerScript layer로 시작함)를 해당 레이어 아래에 생성.

일단 효과 적용 레이어가 생성되면 링크 표시는 없어도 됨.

태그의 색(color) 옵션에 색 등록 번호를 사용하려면,
먼저 "Modify Color Registers(색상 등록부 수정)" 메뉴를 사용해서 등록 번호(0~255)에
색을 지정해야 함.

색상 등록부(등록 항목)는 개별 레이어용임. (전체 레이어용이 아님.)

**예시 이미지**

<!-- img width="800" height="708" alt="LayerScript-Test" src="https://github.com/user-attachments/assets/3b7a2cfe-4539-4077-a1dc-cc08792886dd" / -->

![예시 이미지](https://github.com/user-attachments/assets/3b7a2cfe-4539-4077-a1dc-cc08792886dd)

| 예시 이미지의 레이어
| -
| 레이어 \{dropshadow:(244.4,211.7,46.5)}
| LayerScript layer #33.0.0
| 윌버 \{alpha\|fill\|move:10:10}
| LayerScript layer #3.0.0
| 피망 \{alpha\|fill:2\|move:100:100}
| LayerScript layer #2.0.0
| 인물화 \{alpha\|grow:3\|fill}
| LayerScript layer #32.0.0
| Stars


### 메뉴

- Script-Fu &rarr; LayerScript
    - Modify color registers...   (색 등록부 수정)
    - Process LayerScript tags    (LayerScript 태그 처리)

### 태그

1. \{alpha\}
    * 설명: 레이어 내용 선택
1. \{all\}
    * 설명: 전체 선택
1. \{none\}
    * 설명: 모든 선택 영역 해제.
1. \{invert\}
    * 설명: 선택 영역 반전
1. {grow:스텝}
    * 설명: 선택 영역 확장
    * 옵션:
        + 스텝: 유형=정수, 기본값=1, 음수를 사용하면 선택 영역 축소
1. \{feather:반지름\}
    * 설명: 선택 영역 페더 효과 적용
    * 옵션:
        + 반지름: 타입=정수, 기본값=1
1. \{smove:x:y\}
    * 설명: 선택 영역 이동(translate)
    * 옵션:
        + x : 타입=실수, 기본값=0
        + y : 타입=실수, 기본값=0
1. \{lbox:모드\}
    * 설명: 페더(feather) 효과를 끄고, 레이어 박스 선택.
    * 옵션:
        + 모드:
            - \+ 또는 add : 현재 선택 영역에 추가
            - \- 또는 subtract : 현재 선택 영역에서 빼기
            - = 또는 replace : 선택 영역 교체
            - ^ 또는 x 또는 intersect : 현재 선택 영역과 레이어 박스가 겹치는 부분 선택
1. \{sbox: 모드\}
    * 설명: 현재 선택 영역이 있으면, 현재 선택 영역 경계 박스를 페더(feather) 효과를 끄고 선택.
    * 옵션:
        + 모드:
            - \+ 또는 add : 현재 선택 영역에 추가
            - \- 또는 subtract : 현재 선택 영역에서 빼기
            - = 또는 replace : 선택 영역 교체
            - ^ 또는 x 또는 intersect : 현재 선택 영역과 레이어 박스가 겹치는 부분 선택
1. \{fade:레벨:선택검사\}
    * 설명: 색&rarr;레벨 효과 적용.
    * 옵션:
        + 레벨:
            - *색 &rarr; 레벨* 효과에서 *출력 레벨(Output Levels)*의 높은 출력(High Output) 강도.
            - 타입=정수, 범위=0~255, 기본값=128
        + 선택검사:
            - 선택검사=0 이면, 현재 선택 영역 검사 안함
            - 선택검사=1 이면, 현재 선택 영역이 있을 때(non-empty)만 레벨 효과 적용.
            - 기본값 = 0
1. \{copy:선택검사\}
    * 설명: 현재 선택 복사 및 붙여 넣기.
    * 옵션:
        + 선택검사:
            - 선택검사=0 이면, 현재 선택 영역 검사 안함
            - 선택검사=1 이면, 현재 선택 영역이 있을 때(non-empty)만 복사 및 붙여넣기 적용.
            - 기본값 = 0
1. \{fill:색:선택검사\}
    * 설명: 선택 영역을 현재 전경색 또는 지정한 번호에 등록된 색으로 채우기
    * 옵션:
        + 색:
            - "Modify Color Register(색 레지스터 수정)" 메뉴로 등록한 번호. 0 에서 255까지
            - (r,g,b): r, g, b 는 실수 또는 정수. 범위는 각각 0~255
            - 기본값 = 0
        + 선택검사:
            - 선택검사=0 이면, 현재 선택 영역 검사 안함
            - 선택검사=1 이면, 현재 선택 영역이 있을 때(non-empty)만 지정한 색으로 채움
            - 기본값 = 0
1. \{clear:선택검사\}
    * 설명: 현재 선택 영역 안의 내용 삭제.
    * 옵션:
        + 선택검사:
            - 선택검사=0 이면, 현재 선택 영역 검사 안함
            - 선택검사=1 이면, 현재 선택 영역이 있을 때(non-empty)만 삭제.
            - 기본값 = 0
1. \{blurshape:색:처음값:크기:반전\}
    * 설명:
    * 옵션:
        + 색:
            - "Modify Color Register(색 레지스터 수정)" 메뉴로 등록한 번호. 0 에서 255까지
            - (r,g,b): r, g, b 실수 또는 정수. 범위는 각각 0~255
            - 기본값 = 0
        + 처음값: 기본값=3
        + 크기: 기본값=5
        + 반전: 기본값= 0
1. \{dropshadow:색:불투명도:크기:이격각도:이격거리\}
    * 설명:
    * 옵션:
        + 색:
            - "Modify Color Register(색 레지스터 수정)" 메뉴로 등록한 번호. 0 에서 255까지
            - (r,g,b): r, g, b는 실수 또는 정수. 범위는 각각 0~255
            - 기본값 = (0,0,0)
        + 불투명도: 범위=0~100, 기본값=75
        + 크기: 타입=정수, 기본값=5
        + 이격각도(offset-angle): 타입=실수, 기본값=120
        + 이격거리(offset-distance): 타입=실수, 기본값=5
1. \{gff:단계수:반지름:색\}
    * 설명: 선택 확장(grow), 선택 가장자리 모호하게(feather), 색 채우기(fill)을 차례로 적용.
    * 옵션:
        + 단계수: 기본값=1
        + 반지름: 기본값=1
        + 색:
            - "Modify Color Register(색 레지스터 수정)" 메뉴로 등록한 번호. 1 에서 255까지
            - (r,g,b): r, g, b는 실수 또는 정수. 범위는 0~255
            - 기본값 = 0
1. \{opacity:불투명도\}
    * 설명: 레이어의 불투명도 변경
    * 옵션:
        + 불투명도: 기본값=100
1. \{move:x:y\}
    * 설명: 레이어 이동(translate)
    * 옵션:
        + x : 타입=실수, 기본값=0
        + y : 타입=실수, 기본값=0
