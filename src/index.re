open Reprocessing;

type stateT('instance) = {
  global: Global.stateT,
  instance: 'instance
};

let entity = Entity.instance(Protagonist.make())
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(-4., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(-3., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(-2., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(0., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(2., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(3., 3.))))
  |> Entity.composeInstances(Entity.instance(FloorTile.make(~position=(4., 3.))));

let setup = (env) => {
  let global = Global.setup(env);
  let instance = entity.setup(global, env);
  { global: global, instance: instance }
};

let draw = ({ global: prevGlobal, instance: prevInstance }, env) => {
  let global = Global.step(prevGlobal, env);
  let instance = entity.step(prevInstance, global, env);
  let global = Global.bodyResolution(global, env);
  Global.draw(global, env);
  entity.draw(instance, global, env);
  { global: global, instance: instance }
};

run(~setup, ~draw, ());
