[%bs.raw {|require('../../../src/Container.scss')|}];

type action =
  | StartInteraction(int)
  | MoveInteraction(int)
  | EndInteraction(int)
  | Spin(float);

type values = {
  initial: ref(float),
  final: ref(float),
  current: ref(float),
  previous: ref(float),
};

type velocity = {
  current: ref(float),
  list: ref(list(float)),
};

type state = {
  rotation: ref(float),
  isMouseDown: ref(bool),
  hasUserInteractionEnded: bool,
  position: values,
  time: values,
  transform: string,
  velocity,
};

[@bs.val]
external requestAnimationFrame: (unit => unit) => int =
  "requestAnimationFrame";

[@bs.val] external cancelAnimationFrame: int => unit = "cancelAnimationFrame";

[@bs.val] [@bs.scope "performance"] external now: unit => float = "now";

let averageLatestNonZeroVelocities = (velocities, n) => {
  let sum = list => {
    let rec aux = (acc, list) => {
      switch (list) {
      | [] => acc
      | [hd, ...tl] => aux(acc +. hd, tl)
      };
    };
    aux(0.0, list);
  };

  switch (Belt.List.(keep(velocities, v => v !== 0.0)->take(3))) {
  | None => 0.0
  | Some(list) => sum(list) /. float_of_int(n)
  };
};

let calculateFrictionOffset =
    (~state, ~numberOfSides, ~amountOfFriction, ~shouldRoundToNearestSide) =>
  if (shouldRoundToNearestSide) {
    let iterations =
      int_of_float(
        Js.Math.abs_float(state.velocity.current^ /. amountOfFriction),
      );
    let sign = state.velocity.current^ < 0.0 ? 1.0 : (-1.0);
    let estimatedFinalRotation =
      state.rotation^
      +. float_of_int(iterations / 2)
      *. (
        2.0
        *. state.velocity.current^
        +. float_of_int(iterations - 1)
        *. sign
        *. amountOfFriction
      );
    let degreesPerSide = 360.0 /. float_of_int(numberOfSides);
    let roundedFinalRotation =
      Js.Math.round(estimatedFinalRotation /. degreesPerSide)
      *. degreesPerSide;
    (estimatedFinalRotation -. roundedFinalRotation)
    /. float_of_int(iterations);
  } else {
    0.0;
  };

let spinWithFriction =
    (
      ~state,
      ~dispatch,
      ~numberOfSides,
      ~amountOfFriction,
      ~shouldRoundToNearestSide,
    ) => {
  let amountOfFriction = float_of_int(amountOfFriction) /. 1000. +. 0.005;
  switch (state.velocity.current^) {
  | velocity when Js.Math.abs_float(velocity) > amountOfFriction =>
    let velocity =
      velocity > 0.0
        ? velocity -. amountOfFriction : velocity +. amountOfFriction;
    let offset =
      calculateFrictionOffset(
        ~state,
        ~numberOfSides,
        ~amountOfFriction,
        ~shouldRoundToNearestSide,
      );
    dispatch(Spin(velocity -. offset));
  | _ =>
    let degreesPerSide = 360.0 /. float_of_int(numberOfSides);
    let currentSide =
      if (state.rotation^ < 0.0) {
        int_of_float(
          Js.Math.round(
            Js.Math.abs_float(state.rotation^) /. degreesPerSide,
          ),
        )
        mod numberOfSides;
      } else {
        (
          numberOfSides
          - int_of_float(
              Js.Math.round(
                Js.Math.abs_float(state.rotation^) /. degreesPerSide,
              ),
            )
          mod numberOfSides
        )
        mod numberOfSides;
      };
    Js.log(
      "You've landed on side " ++ string_of_int(currentSide + 1) ++ ".",
    );
  };
};

let buildTransformString = (~radius, ~rotation) => {
  "translate3d(0, 0, -"
  ++ Js.Float.toString(radius)
  ++ "vw) rotateY("
  ++ Js.Float.toString(rotation)
  ++ "deg)";
};

[@react.component]
let make =
    (
      ~numberOfSides,
      ~radius,
      ~perspective,
      ~amountOfFriction,
      ~shouldRoundToNearestSide,
    ) => {
  let (state, dispatch) =
    React.useReducer(
      (state, action) =>
        switch (action) {
        | StartInteraction(clientX) =>
          state.isMouseDown := true;
          state.position.previous := float_of_int(clientX);
          state.time.initial := now();
          state.time.previous := now();
          state.velocity.current := 0.0;
          state.velocity.list := [];
          {...state, hasUserInteractionEnded: false};
        | MoveInteraction(clientX) =>
          state.isMouseDown^
            ? {
              state.position.current := float_of_int(clientX);
              state.time.current := now();
              state.rotation :=
                state.rotation^
                -. (state.position.previous^ -. state.position.current^)
                /. float_of_int(numberOfSides);
              let dx = state.position.current^ -. state.position.previous^;
              let dt = state.time.current^ -. state.time.previous^;
              state.velocity.current := dx /. dt;
              state.velocity.list :=
                [state.velocity.current^, ...state.velocity.list^];
              state.position.previous := state.position.current^;
              state.time.previous := state.time.current^;
              {
                ...state,
                transform:
                  buildTransformString(~radius, ~rotation=state.rotation^),
              };
            }
            : state
        | EndInteraction(clientX) =>
          state.isMouseDown := false;
          state.position.final := float_of_int(clientX);
          state.time.final := now();
          state.velocity.current :=
            averageLatestNonZeroVelocities(state.velocity.list^, 3);
          {...state, hasUserInteractionEnded: true};
        | Spin(velocity) =>
          state.rotation := state.rotation^ +. velocity;
          state.velocity.current := velocity;
          {
            ...state,
            transform:
              buildTransformString(~radius, ~rotation=state.rotation^),
          };
        },
      {
        {
          rotation: ref(0.0),
          isMouseDown: ref(false),
          hasUserInteractionEnded: false,
          position: {
            initial: ref(0.0),
            final: ref(0.0),
            current: ref(0.0),
            previous: ref(0.0),
          },
          time: {
            initial: ref(0.0),
            final: ref(0.0),
            current: ref(0.0),
            previous: ref(0.0),
          },
          transform: buildTransformString(~radius, ~rotation=0.0),
          velocity: {
            current: ref(0.0),
            list: ref([]),
          },
        };
      },
    );

  React.useLayoutEffect(() => {
    if (state.hasUserInteractionEnded) {
      requestAnimationFrame(() =>
        spinWithFriction(
          ~state,
          ~dispatch,
          ~numberOfSides,
          ~amountOfFriction,
          ~shouldRoundToNearestSide,
        )
      )
      ->ignore;
    };

    None;
  });

  <div
    id="container"
    onMouseDown={event =>
      dispatch(StartInteraction(ReactEvent.Mouse.clientX(event)))
    }
    onMouseMove={event =>
      dispatch(MoveInteraction(ReactEvent.Mouse.clientX(event)))
    }
    onMouseUp={event =>
      dispatch(EndInteraction(ReactEvent.Mouse.clientX(event)))
    }
    onTouchStart={event =>
      dispatch(
        StartInteraction(
          ReactEvent.Touch.changedTouches(event)##item(0)##clientX,
        ),
      )
    }
    onTouchMove={event =>
      dispatch(
        MoveInteraction(
          ReactEvent.Touch.changedTouches(event)##item(0)##clientX,
        ),
      )
    }
    onTouchEnd={event =>
      dispatch(
        EndInteraction(
          ReactEvent.Touch.changedTouches(event)##item(0)##clientX,
        ),
      )
    }
    style={ReactDOMRe.Style.make(~perspective, ())}>
    <Carousel
      numberOfSides
      radius
      transform={buildTransformString(~radius, ~rotation=state.rotation^)}
    />
  </div>;
};