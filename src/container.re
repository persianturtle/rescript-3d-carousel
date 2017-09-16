[%bs.raw {|require('./container.css')|}];

type action =
  | StartInteraction int
  | MoveInteraction int
  | EndInteraction int
  | Spin float;

type values = {
  initial: ref float,
  final: ref float,
  current: ref float,
  previous: ref float
};

type velocity = {
  current: ref float,
  list: ref (list float)
};

type css = {
  perspective: string,
  transform: string
};

type state = {
  rotation: ref float,
  radius: float,
  isMouseDown: ref bool,
  position: values,
  time: values,
  velocity,
  css,
  requestAnimationFrameID: ref int
};

type retainedProps = {
  sides: int,
  friction: float
};

external requestAnimationFrame : (unit => unit) => int = "requestAnimationFrame" [@@bs.val];

external cancelAnimationFrame : int => unit = "cancelAnimationFrame" [@@bs.val];

external now : unit => float = "now" [@@bs.val] [@@bs.scope "performance"];

external abs : float => float = "abs" [@@bs.val] [@@bs.scope "Math"];

external round : float => int = "round" [@@bs.val] [@@bs.scope "Math"];

external unsafeAnyToArray : 'a => array 'a = "%identity";

let component = ReasonReact.reducerComponentWithRetainedProps "Container";

let findAndFilter ::list ::n ::f => {
  let rec aux list' n' acc =>
    if (n' == 0) {
      acc
    } else {
      switch list' {
      | [] => []
      | [item, ...rest] =>
        if (f item) {
          aux rest (n' - 1) [item, ...acc]
        } else {
          aux rest n' acc
        }
      }
    };
  aux list n []
};

let rec sum list =>
  switch list {
  | [] => 0.0
  | [item, ...rest] => item +. sum rest
  };

let averageLatestNonzeroVelocities velocities n => {
  let latestNonzeroVelocities = findAndFilter list::velocities ::n f::(fun item => item != 0.0);
  sum latestNonzeroVelocities /. float_of_int n
};

let calculateOffset state friction sides roundToNearestSide =>
  if roundToNearestSide {
    let iterations = int_of_float (abs (!state.velocity.current /. friction));
    let sign = !state.velocity.current < 0.0 ? 1.0 : (-1.0);
    let estimatedFinalRotation =
      !state.rotation
      +. float_of_int (iterations / 2)
      *. (2.0 *. !state.velocity.current +. float_of_int (iterations - 1) *. sign *. friction);
    let degreesPerSide = 360.0 /. float_of_int sides;
    let roundedFinalRotation =
      float_of_int (round (estimatedFinalRotation /. degreesPerSide)) *. degreesPerSide;
    (estimatedFinalRotation -. roundedFinalRotation) /. float_of_int iterations
  } else {
    0.0
  };

let spinWithFriction state reduce friction sides roundToNearestSide => {
  let offset = calculateOffset state friction sides roundToNearestSide;
  let rec onAnimationFrame velocity' () => {
    state.velocity.current := velocity';
    if !state.isMouseDown {
      Js.log "The carousel has previously been spun."
    } else if (
      abs velocity' < friction
    ) {
      cancelAnimationFrame !state.requestAnimationFrameID;
      let degreesPerSide = 360.0 /. float_of_int sides;
      let currentSide =
        if (!state.rotation < 0.0) {
          round (abs !state.rotation /. degreesPerSide) mod sides
        } else {
          (sides - round (abs !state.rotation /. degreesPerSide) mod sides) mod sides
        };
      Js.log ("You've landed on side " ^ string_of_int (currentSide + 1) ^ ".")
    } else {
      reduce (fun _ => Spin (velocity' -. offset)) ();
      state.requestAnimationFrameID :=
        requestAnimationFrame (
          onAnimationFrame (velocity' > 0.0 ? velocity' -. friction : velocity' +. friction)
        )
    }
  };
  state.requestAnimationFrameID := requestAnimationFrame (onAnimationFrame !state.velocity.current)
};

let make ::sides ::friction ::roundToNearestSide _children => {
  ...component,
  retainedProps: {sides, friction},
  willReceiveProps: fun self =>
    if (self.retainedProps.sides === sides && self.retainedProps.friction === friction) {
      self.state
    } else {
      let radius = 25.0 /. Js.Math.tan (180.0 /. float_of_int sides *. (Js.Math._PI /. 180.0));
      let css = {
        perspective: string_of_float (500.0 /. float_of_int sides) ^ "0vw",
        transform:
          "translate3d(0, 0, -"
          ^ string_of_float radius
          ^ "0vw) rotateY("
          ^ string_of_float 0.0
          ^ "0deg)"
      };
      cancelAnimationFrame !self.state.requestAnimationFrameID;
      spinWithFriction self.state self.reduce friction sides roundToNearestSide;
      {...self.state, radius, css}
    },
  initialState: fun () => {
    let radius = 25.0 /. Js.Math.tan (180.0 /. float_of_int sides *. (Js.Math._PI /. 180.0));
    {
      radius,
      rotation: ref 0.0,
      isMouseDown: ref false,
      position: {initial: ref 0.0, final: ref 0.0, current: ref 0.0, previous: ref 0.0},
      time: {initial: ref 0.0, final: ref 0.0, current: ref 0.0, previous: ref 0.0},
      css: {
        perspective: string_of_float (500.0 /. float_of_int sides) ^ "0vw",
        transform:
          "translate3d(0, 0, -"
          ^ string_of_float radius
          ^ "0vw) rotateY("
          ^ string_of_float 0.0
          ^ "0deg)"
      },
      velocity: {current: ref 0.0, list: ref []},
      requestAnimationFrameID: ref 0
    }
  },
  reducer: fun action state =>
    switch action {
    | StartInteraction clientX =>
      state.isMouseDown := true;
      state.position.previous := float_of_int clientX;
      state.time.initial := now ();
      state.time.previous := now ();
      state.velocity.current := 0.0;
      state.velocity.list := [];
      ReasonReact.NoUpdate
    | MoveInteraction clientX =>
      !state.isMouseDown ?
        {
          state.position.current := float_of_int clientX;
          state.time.current := now ();
          state.rotation :=
            !state.rotation
            -. (!state.position.previous -. !state.position.current)
            /. float_of_int (sides * 2);
          let transform =
            "translate3d(0, 0, -"
            ^ string_of_float state.radius
            ^ "0vw) rotateY("
            ^ string_of_float !state.rotation
            ^ "0deg)";
          let dx = !state.position.current -. !state.position.previous;
          let dt = !state.time.current -. !state.time.previous;
          state.velocity.current := dx /. dt;
          state.velocity.list := [!state.velocity.current, ...!state.velocity.list];
          state.position.previous := !state.position.current;
          state.time.previous := !state.time.current;
          ReasonReact.Update {...state, css: {perspective: state.css.perspective, transform}}
        } :
        ReasonReact.NoUpdate
    | EndInteraction clientX =>
      state.isMouseDown := false;
      state.position.final := float_of_int clientX;
      state.time.final := now ();
      state.velocity.current := averageLatestNonzeroVelocities !state.velocity.list 3;
      ReasonReact.SideEffects (
        fun self => spinWithFriction self.state self.reduce friction sides roundToNearestSide
      )
    | Spin velocity =>
      state.rotation := !state.rotation +. velocity;
      let transform =
        "translate3d(0, 0, -"
        ^ string_of_float state.radius
        ^ "0vw) rotateY("
        ^ string_of_float !state.rotation
        ^ "0deg)";
      ReasonReact.Update {...state, css: {perspective: state.css.perspective, transform}}
    },
  render: fun self =>
    <div
      id="container"
      onMouseDown=(self.reduce (fun event => StartInteraction (ReactEventRe.Mouse.clientX event)))
      onMouseMove=(self.reduce (fun event => MoveInteraction (ReactEventRe.Mouse.clientX event)))
      onMouseUp=(self.reduce (fun event => EndInteraction (ReactEventRe.Mouse.clientX event)))
      onTouchStart=(
        self.reduce (
          fun event =>
            StartInteraction (unsafeAnyToArray (ReactEventRe.Touch.changedTouches event)).(0)##clientX
        )
      )
      onTouchMove=(
        self.reduce (
          fun event =>
            MoveInteraction (unsafeAnyToArray (ReactEventRe.Touch.changedTouches event)).(0)##clientX
        )
      )
      onTouchEnd=(
        self.reduce (
          fun event =>
            EndInteraction (unsafeAnyToArray (ReactEventRe.Touch.changedTouches event)).(0)##clientX
        )
      )
      style=(ReactDOMRe.Style.make perspective::self.state.css.perspective ())>
      <Carousel sides radius=self.state.radius transform=self.state.css.transform />
    </div>
};
