open Reprocessing;

let initState = (_env, _global) => ();

let initAssets = (_env) => ();

let action = (_state, _body, _env) => ();

let entity = Entity.makeEntity(~name="Floor", ~initState, ~initAssets, ~action);

let make = () => {
  ...entity,
  initBody: (_env, _global) => {
    let body = Body.makeBody(
      ~position=(-4., 2.),
      ~mass=0.,
      ~restitution=0.2,
      ~size=(8, 1),
      ~staticFriction=0.05,
      ~dynamicFriction=0.35
    );
    { ...body, gravityFactor: 0. }
  },
  render: (_state, body, _assets, env) => {
    let (width, height) = Body.getPixelSize(body);
    let (x, y) = Body.getPixelPosition(body);

    Draw.pushMatrix(env);
    Draw.translate(~x, ~y, env);
    Draw.fill(Utils.color(~r=39, ~g=60, ~b=117, ~a=255), env);
    Draw.rect(~pos=(0, 0), ~width, ~height, env);
    Draw.popMatrix(env);
  }
};
