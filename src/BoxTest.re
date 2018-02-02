open Reprocessing;

let initState = (_env, _global) => ();

let initAssets = (_env) => ();

let action = (_state, _body, _env) => ();

let entity = Entity.makeEntity(~name="BoxTest", ~initState, ~initAssets, ~action);

let make = (~position: (float, float)) => {
  ...entity,
  initBody: (_env, _global) => {
    Body.makeBody(
      ~position,
      ~mass=30.,
      ~restitution=0.05,
      ~size=(1., 1.),
      ~friction=0.2
    );
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
