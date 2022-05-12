%%raw(`import "../../../src/Container.scss"`)

@val external location: 'a = "location"

let team: Js.Array2.t<string> = location["search"]["split"](. "=")["1"]["split"](. ",")
let numberOfSides = Js.Array2.length(team)
let radius = 25.0 /. Js.Math.tan(180.0 /. float_of_int(numberOfSides) *. (Js.Math._PI /. 180.0))
let perspective = Js.Float.toString(500.0 /. float_of_int(numberOfSides)) ++ "vw"

type action =
  | StartInteraction(int)
  | MoveInteraction(int)
  | EndInteraction(int)
  | Spin(float)

type values = {
  initial: ref<float>,
  final: ref<float>,
  current: ref<float>,
  previous: ref<float>,
}

type velocity = {
  current: ref<float>,
  list: ref<array<float>>,
}

type state = {
  rotation: ref<float>,
  isMouseDown: ref<bool>,
  hasUserInteractionEnded: bool,
  position: values,
  time: values,
  transform: string,
  velocity: velocity,
}

@val
external requestAnimationFrame: (float => unit) => int = "requestAnimationFrame"

@val @scope("performance") external now: unit => float = "now"

let averageLatestNonZeroVelocities = (velocities, n) => {
  let sum = Js.Array2.reduce(
    velocities->Js.Array2.filter(v => v > 0.0)->Js.Array2.slice(~start=1, ~end_=3),
    (sum, v) => {
      v +. sum
    },
    0.0,
  )

  sum /. float_of_int(n)
}

/* math.stackexchange.com/q/2429749/61853 */
let calculateFrictionOffset = (
  ~state,
  ~numberOfSides,
  ~amountOfFriction,
  ~shouldRoundToNearestSide,
) =>
  if shouldRoundToNearestSide {
    let iterations = int_of_float(
      Js.Math.abs_float(state.velocity.current.contents /. amountOfFriction),
    )
    let sign = state.velocity.current.contents < 0.0 ? 1.0 : -1.0
    let estimatedFinalRotation =
      state.rotation.contents +.
      float_of_int(iterations / 2) *.
      (2.0 *. state.velocity.current.contents +.
        float_of_int(iterations - 1) *. sign *. amountOfFriction)
    let degreesPerSide = 360.0 /. float_of_int(numberOfSides)
    let roundedFinalRotation =
      Js.Math.round(estimatedFinalRotation /. degreesPerSide) *. degreesPerSide
    (estimatedFinalRotation -. roundedFinalRotation) /. float_of_int(iterations)
  } else {
    0.0
  }

let spinWithFriction = (
  ~state,
  ~dispatch,
  ~numberOfSides,
  ~amountOfFriction,
  ~shouldRoundToNearestSide,
) => {
  let amountOfFriction = float_of_int(amountOfFriction) /. 1000. +. 0.005
  switch state.velocity.current.contents {
  | velocity if Js.Math.abs_float(velocity) > amountOfFriction =>
    let velocity = velocity > 0.0 ? velocity -. amountOfFriction : velocity +. amountOfFriction
    let offset = calculateFrictionOffset(
      ~state,
      ~numberOfSides,
      ~amountOfFriction,
      ~shouldRoundToNearestSide,
    )
    dispatch(Spin(velocity -. offset))
  | _ =>
    let degreesPerSide = 360.0 /. float_of_int(numberOfSides)
    let currentSide = if state.rotation.contents < 0.0 {
      mod(
        int_of_float(Js.Math.round(Js.Math.abs_float(state.rotation.contents) /. degreesPerSide)),
        numberOfSides,
      )
    } else {
      mod(
        numberOfSides -
        mod(
          int_of_float(Js.Math.round(Js.Math.abs_float(state.rotation.contents) /. degreesPerSide)),
          numberOfSides,
        ),
        numberOfSides,
      )
    }
    Js.log("You've landed on side " ++ (string_of_int(currentSide + 1) ++ "."))
  }
}

let buildTransformString = (~radius, ~rotation) =>
  "translate3d(0, 0, -" ++
  (Js.Float.toString(radius) ++
  ("vw) rotateY(" ++ (Js.Float.toString(rotation) ++ "deg)")))

@react.component
let make = (~amountOfFriction, ~shouldRoundToNearestSide) => {
  let (state, dispatch) = React.useReducer(
    (state, action) =>
      switch action {
      | StartInteraction(clientX) =>
        state.isMouseDown := true
        state.position.previous := float_of_int(clientX)
        state.time.initial := now()
        state.time.previous := now()
        state.velocity.current := 0.0
        state.velocity.list := []
        {...state, hasUserInteractionEnded: false}
      | MoveInteraction(clientX) =>
        state.isMouseDown.contents
          ? {
              state.position.current := float_of_int(clientX)
              state.time.current := now()
              state.rotation :=
                state.rotation.contents -.
                (state.position.previous.contents -. state.position.current.contents) /.
                  float_of_int(numberOfSides)
              let dx = state.position.current.contents -. state.position.previous.contents
              let dt = state.time.current.contents -. state.time.previous.contents
              state.velocity.current := dx /. dt
              state.velocity.list :=
                Js.Array2.concat([state.velocity.current.contents], state.velocity.list.contents)
              state.position.previous := state.position.current.contents
              state.time.previous := state.time.current.contents
              {
                ...state,
                transform: buildTransformString(~radius, ~rotation=state.rotation.contents),
              }
            }
          : state
      | EndInteraction(clientX) =>
        state.isMouseDown := false
        state.position.final := float_of_int(clientX)
        state.time.final := now()
        state.velocity.current := averageLatestNonZeroVelocities(state.velocity.list.contents, 3)
        {...state, hasUserInteractionEnded: true}
      | Spin(velocity) =>
        state.rotation := state.rotation.contents +. velocity
        state.velocity.current := velocity
        {
          ...state,
          transform: buildTransformString(~radius, ~rotation=state.rotation.contents),
        }
      },
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
    },
  )

  React.useEffect(() => {
    if state.hasUserInteractionEnded {
      requestAnimationFrame(_timestamp => {
        spinWithFriction(
          ~state,
          ~dispatch,
          ~numberOfSides,
          ~amountOfFriction,
          ~shouldRoundToNearestSide,
        )
      })->ignore
    }

    None
  })

  <div
    id="container"
    onMouseDown={event => dispatch(StartInteraction(ReactEvent.Mouse.clientX(event)))}
    onMouseMove={event => dispatch(MoveInteraction(ReactEvent.Mouse.clientX(event)))}
    onMouseUp={event => dispatch(EndInteraction(ReactEvent.Mouse.clientX(event)))}
    onTouchStart={event =>
      dispatch(StartInteraction(ReactEvent.Touch.changedTouches(event)["item"](0)["clientX"]))}
    onTouchMove={event =>
      dispatch(MoveInteraction(ReactEvent.Touch.changedTouches(event)["item"](0)["clientX"]))}
    onTouchEnd={event =>
      dispatch(EndInteraction(ReactEvent.Touch.changedTouches(event)["item"](0)["clientX"]))}
    style={ReactDOM.Style.make(~perspective, ())}>
    <Carousel
      team radius transform={buildTransformString(~radius, ~rotation=state.rotation.contents)}
    />
  </div>
}
