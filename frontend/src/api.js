import m from "mithril";
import {postStringId} from "./browser.js";

export const incrementLikesCount = () =>  {
  return m.request({
    method: "POST",
    //headers: { "X-real-ip": "0.0.0.0" },
    url: `/blogapi/like/${postStringId}`
  })
}

export const getLikesCount = () => 
  m.request({
    method: "GET",
    url: `/blogapi/likes/${postStringId}`
  })
