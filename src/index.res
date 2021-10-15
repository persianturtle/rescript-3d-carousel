%%raw(` import "../../../src/Index.scss" `)

switch(ReactDOM.querySelector("#root")){
| Some(root) => ReactDOM.render(<App />, root)
| None => ()
}
