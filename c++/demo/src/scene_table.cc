#include "scene.h"

#define ADD_SCENE_TO_TABLE(id) Scene##id::create,

const SceneCreateFunc sSceneTable[] = {
  FOR_EACH_SCENE(ADD_SCENE_TO_TABLE)
};
