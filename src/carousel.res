%%raw(`import "../../../src/Carousel.scss"`)

@react.component
let make = (~team, ~radius, ~transform) =>
  <section id="carousel" style={ReactDOM.Style.make(~transform, ())}>
    {Js.Array2.mapi(team, (teamMember, index) => {
      let css =
        "rotate3d(0, 1, 0," ++
        (Js.Float.toString(360.0 *. float_of_int(index) /. float_of_int(Js.Array2.length(team))) ++
        ("deg) translate3d(0, 0," ++ (Js.Float.toString(radius) ++ "vw)")))

      <figure key={string_of_int(index)} style={ReactDOM.Style.make(~transform=css, ())}>
        {React.string(teamMember)}
      </figure>
    })->React.array}
  </section>
