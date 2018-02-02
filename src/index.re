open Reprocessing;

type stateT('instance) = {
  acc: float,
  global: Global.stateT,
  instance: 'instance
};

let entity = Entity.instance(Protagonist.make())
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(-8., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(-7., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(-6., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(-5., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(-4., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(-3., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(-2., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(-1., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(0., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(2., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(3., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(4., 3.))))
  |> Entity.composeInstances(Entity.instance(BoxTest.make(~position=(3.5, 2.))))
  |> Entity.composeInstances(Entity.instance(BoxTest.make(~position=(3.5, 1.))));

let setup = (env) => {
  let global = Global.setup(env);
  let instance = entity.setup(global, env);
  { acc: 0., global: global, instance: instance }
};

let dt = 1. /. 60.;

let draw = ({ acc: prevAcc, global: prevGlobal, instance: prevInstance }, env) => {
  let global = ref(Global.step(prevGlobal, env));
  let instance = entity.step(prevInstance, global^, env);
  let frameTime = min(Env.deltaTime(env), 0.25);

  let acc = ref(prevAcc +. frameTime);
  while (acc^ >= dt) {
    global := Global.bodyResolution(global^, dt);
    acc := acc^ -. dt;
  };

  Global.draw(global^, env);
  entity.draw(instance, global^, env);

  { acc: acc^, global: global^, instance: instance }
};

run(~setup, ~draw, ());
