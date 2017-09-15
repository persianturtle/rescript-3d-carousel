[%bs.raw {|require('./index.css')|}];

external register_service_worker : unit => unit =
  "default" [@@bs.module "./registerServiceWorker"];

ReactDOMRe.renderToElementWithId <App sides=8 friction=0.0175 roundToNearestSide=true /> "root";

register_service_worker ();
