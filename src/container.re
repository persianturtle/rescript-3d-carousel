[%bs.raw {|require('./container.css')|}];

type action =
  | StartInteraction(int)
  | MoveInteraction(int)
  | EndInteraction(int)
  | Spin(float);

type values = {
  initial: ref(float),
  final: ref(float),
  current: ref(float),
  previous: ref(float)
};

type velocity = {
  current: ref(float),
  list: ref(list(float))
};

type css = {
  perspective: string,
  transform: string
};

type state = {
  requestAnimationFrameID: ref(int),
  rotation: ref(float),
  radius: float,
  isMouseDown: ref(bool),
  position: values,
  time: values,
  velocity,
  css
};

type retainedProps = {
  sides: int,
  friction: float,
  roundToNearestSide: bool
};

[@bs.val] external requestAnimationFrame : (unit => unit) => int = "requestAnimationFrame";

[@bs.val] external cancelAnimationFrame : int => unit = "cancelAnimationFrame";

[@bs.val] [@bs.scope "performance"] external now : unit => float = "now";

/*
   @yawaramin mentioned:
   re: your open question about TouchList conversion to array,
   i'd model TouchList as a separate module in TouchList.re,
   and bind to the TouchList.item method with an external since you only
   seem to be getting the first touch item from the touchlist. so like
   /* TouchList.re */ type t; external item : int => _ = "" [@@bs.send.pipe: t];(edited)
   instead of unsafeAnyToArray (ReactEventRe.Touch.changedTouches event)).(0)##clientX
   you can do (event |> ReactEventRe.Touch.changedTouches |> TouchList.item 0)##clientX
 */
external unsafeAnyToArray : 'a => array('a) = "%identity";

let component = ReasonReact.reducerComponentWithRetainedProps("Container");

let findAndFilter = (~list, ~n, ~f) => {
  let rec aux = (list', n', acc) =>
    if (n' == 0) {
      acc
    } else {
      switch list' {
      | [] => []
      | [item, ...rest] =>
        if (f(item)) {
          aux(rest, n' - 1, [item, ...acc])
        } else {
          aux(rest, n', acc)
        }
      }
    };
  aux(list, n, [])
};

let rec sum = (list) =>
  switch list {
  | [] => 0.0
  | [item, ...rest] => item +. sum(rest)
  };

let averageLatestNonzeroVelocities = (velocities, n) => {
  let latestNonzeroVelocities = findAndFilter(~list=velocities, ~n, ~f=(item) => item != 0.0);
  sum(latestNonzeroVelocities) /. float_of_int(n)
};

let calculateOffset = (state, friction, sides, roundToNearestSide) =>
  if (roundToNearestSide) {
    let iterations = int_of_float(Js.Math.abs_float(state.velocity.current^ /. friction));
    let sign = state.velocity.current^ < 0.0 ? 1.0 : (-1.0);
    let estimatedFinalRotation =
      state.rotation^
      +. float_of_int(iterations / 2)
      *. (2.0 *. state.velocity.current^ +. float_of_int(iterations - 1) *. sign *. friction);
    let degreesPerSide = 360.0 /. float_of_int(sides);
    let roundedFinalRotation =
      Js.Math.round(estimatedFinalRotation /. degreesPerSide) *. degreesPerSide;
    (estimatedFinalRotation -. roundedFinalRotation) /. float_of_int(iterations)
  } else {
    0.0
  };

let spinWithFriction = (state, reduce, friction, sides, roundToNearestSide) => {
  let offset = calculateOffset(state, friction, sides, roundToNearestSide);
  let rec onAnimationFrame = (velocity', ()) => {
    state.velocity.current := velocity';
    if (state.isMouseDown^) {
      Js.log("The carousel has previously been spun.")
    } else if (Js.Math.abs_float(velocity') < friction) {
      cancelAnimationFrame(state.requestAnimationFrameID^);
      let degreesPerSide = 360.0 /. float_of_int(sides);
      let currentSide =
        if (state.rotation^ < 0.0) {
          int_of_float(Js.Math.round(Js.Math.abs_float(state.rotation^) /. degreesPerSide))
          mod sides
        } else {
          (
            sides
            - int_of_float(Js.Math.round(Js.Math.abs_float(state.rotation^) /. degreesPerSide))
            mod sides
          )
          mod sides
        };
      Js.log("You've landed on side " ++ (string_of_int(currentSide + 1) ++ "."))
    } else {
      Js.log(velocity');
      reduce((_) => Spin(velocity' -. offset), ());
      state.requestAnimationFrameID :=
        requestAnimationFrame(
          onAnimationFrame(velocity' > 0.0 ? velocity' -. friction : velocity' +. friction)
        )
    }
  };
  state.requestAnimationFrameID := requestAnimationFrame(onAnimationFrame(state.velocity.current^))
};

let make = (~sides, ~friction, ~roundToNearestSide, _children) => {
  ...component,
  retainedProps: {sides, friction, roundToNearestSide},
  willReceiveProps: (self) =>
    if (self.retainedProps.sides === sides
        && self.retainedProps.friction === friction
        && self.retainedProps.roundToNearestSide === roundToNearestSide) {
      self.state
    } else {
      let radius = 25.0 /. Js.Math.tan(180.0 /. float_of_int(sides) *. (Js.Math._PI /. 180.0));
      let css = {
        perspective: string_of_float(500.0 /. float_of_int(sides)) ++ "0vw",
        transform:
          "translate3d(0, 0, -"
          ++ (
            string_of_float(radius)
            ++ ("0vw) rotateY(" ++ (string_of_float(self.state.rotation^) ++ "0deg)"))
          )
      };
      cancelAnimationFrame(self.state.requestAnimationFrameID^);
      spinWithFriction(self.state, self.reduce, friction, sides, roundToNearestSide);
      {...self.state, radius, css}
    },
  initialState: () => {
    let radius = 25.0 /. Js.Math.tan(180.0 /. float_of_int(sides) *. (Js.Math._PI /. 180.0));
    {
      requestAnimationFrameID: ref(0),
      radius,
      rotation: ref(0.0),
      isMouseDown: ref(false),
      position: {initial: ref(0.0), final: ref(0.0), current: ref(0.0), previous: ref(0.0)},
      time: {initial: ref(0.0), final: ref(0.0), current: ref(0.0), previous: ref(0.0)},
      css: {
        perspective: string_of_float(500.0 /. float_of_int(sides)) ++ "0vw",
        transform:
          "translate3d(0, 0, -"
          ++ (string_of_float(radius) ++ ("0vw) rotateY(" ++ (string_of_float(0.0) ++ "0deg)")))
      },
      velocity: {current: ref(0.0), list: ref([])}
    }
  },
  reducer: (action, state) =>
    switch action {
    | StartInteraction(clientX) =>
      state.isMouseDown := true;
      state.position.previous := float_of_int(clientX);
      state.time.initial := now();
      state.time.previous := now();
      state.velocity.current := 0.0;
      state.velocity.list := [];
      ReasonReact.NoUpdate
    | MoveInteraction(clientX) =>
      state.isMouseDown^ ?
        {
          state.position.current := float_of_int(clientX);
          state.time.current := now();
          state.rotation :=
            state.rotation^
            -. (state.position.previous^ -. state.position.current^)
            /. float_of_int(sides * 2);
          let transform =
            "translate3d(0, 0, -"
            ++ (
              string_of_float(state.radius)
              ++ ("0vw) rotateY(" ++ (string_of_float(state.rotation^) ++ "0deg)"))
            );
          let dx = state.position.current^ -. state.position.previous^;
          let dt = state.time.current^ -. state.time.previous^;
          state.velocity.current := dx /. dt;
          state.velocity.list := [state.velocity.current^, ...state.velocity.list^];
          state.position.previous := state.position.current^;
          state.time.previous := state.time.current^;
          ReasonReact.Update({...state, css: {perspective: state.css.perspective, transform}})
        } :
        ReasonReact.NoUpdate
    | EndInteraction(clientX) =>
      state.isMouseDown := false;
      state.position.final := float_of_int(clientX);
      state.time.final := now();
      state.velocity.current := averageLatestNonzeroVelocities(state.velocity.list^, 3);
      ReasonReact.SideEffects(
        ((self) => spinWithFriction(self.state, self.reduce, friction, sides, roundToNearestSide))
      )
    | Spin(velocity) =>
      state.rotation := state.rotation^ +. velocity;
      let transform =
        "translate3d(0, 0, -"
        ++ (
          string_of_float(state.radius)
          ++ ("0vw) rotateY(" ++ (string_of_float(state.rotation^) ++ "0deg)"))
        );
      ReasonReact.Update({...state, css: {perspective: state.css.perspective, transform}})
    },
  render: (self) =>
    <div
      id="container"
      onMouseDown=(self.reduce((event) => StartInteraction(ReactEventRe.Mouse.clientX(event))))
      onMouseMove=(self.reduce((event) => MoveInteraction(ReactEventRe.Mouse.clientX(event))))
      onMouseUp=(self.reduce((event) => EndInteraction(ReactEventRe.Mouse.clientX(event))))
      onTouchStart=(
        self.reduce(
          (event) =>
            StartInteraction(
              unsafeAnyToArray(ReactEventRe.Touch.changedTouches(event))[0]##clientX
            )
        )
      )
      onTouchMove=(
        self.reduce(
          (event) =>
            MoveInteraction(unsafeAnyToArray(ReactEventRe.Touch.changedTouches(event))[0]##clientX)
        )
      )
      onTouchEnd=(
        self.reduce(
          (event) =>
            EndInteraction(unsafeAnyToArray(ReactEventRe.Touch.changedTouches(event))[0]##clientX)
        )
      )
      style=(ReactDOMRe.Style.make(~perspective=self.state.css.perspective, ()))>
      <Carousel sides radius=self.state.radius transform=self.state.css.transform />
    </div>
};
