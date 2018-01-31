open Reprocessing;

type assetsT = {
  concrete: imageT,
};

let initState = (_env, _global) => ();

let initAssets = (env) => {
  concrete: Draw.loadImage(~filename="assets/room/floor.png", ~isPixel=true, env)
};

let action = (_state, _body, _env) => ();

let entity = Entity.makeEntity(~name="FloorTile", ~initState, ~initAssets, ~action);

let make = (~position: (float, float)) => {
  ...entity,
  initBody: (_env, _global) => {
    let body = Body.makeBody(
      ~position,
      ~mass=0.,
      ~restitution=0.2,
      ~size=(1., 1.),
      ~staticFriction=0.05,
      ~dynamicFriction=0.35
    );
    { ...body, gravityFactor: 0. }
  },
  render: (_state, body, assets, env) => {
    let (width, height) = Body.getPixelSize(body);
    let (x, y) = Body.getPixelPosition(body);

    Draw.pushMatrix(env);
    Draw.translate(~x, ~y, env);
    Draw.image(assets.concrete, ~pos=(0, 0), ~width, ~height, env);
    Draw.popMatrix(env);
  }
};
