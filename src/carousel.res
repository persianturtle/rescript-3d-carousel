%%raw(` import "../../../src/Carousel.scss" `)

let create_sides = (numberOfSides, radius) =>
  Array.init(numberOfSides, index => {
    let css =
      "rotate3d(0, 1, 0," ++
      (Js.Float.toString(360.0 *. float_of_int(index) /. float_of_int(numberOfSides)) ++
      ("deg) translate3d(0, 0," ++ (Js.Float.toString(radius) ++ "vw)")))
    <figure key={string_of_int(index)} style={ReactDOM.Style.make(~transform=css, ())}>
      {React.string(string_of_int(index + 1))}
    </figure>
  })

@react.component
let make = (~numberOfSides, ~radius, ~transform) =>
  <section id="carousel" style={ReactDOM.Style.make(~transform, ())}>
    {React.array(create_sides(numberOfSides, radius))}
  </section>
