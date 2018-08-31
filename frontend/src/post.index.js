import m from "mithril";
import app from "./app.js"
import { linkFontFaces } from "./fonts.js"

document.body.onload = () => {
  linkFontFaces();

  const footer = document.getElementById("like-footer")
  m.mount(footer, app);
}


