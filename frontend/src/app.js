import m from "mithril";
import { incrementLikesCount, getLikesCount } from "./api.js"

const LikeCounter = {
  view: ({ attrs: { liked, likesCount } }) => 
    <div id="like-count" className={liked ? "liked" : ""}>
      {likesCount}
    </div>
}

const LikeButton = {
  view: ({ attrs: { liked, onLikeButtonClicked } }) => 
    <div id="heart" 
      style={liked ? {backgroundPosition: "right"} : null} 
      className={liked ? "heart-animation" : ""}
      onclick={onLikeButtonClicked}>
    </div> 
}

const app = () => {
  let liked = false;
  let likesCount = "??";
  let lastCountFromServer = likesCount;
  let footerMsg = "If you liked this post, please hit this like button to show your appreciation.";

  const updateLikesCounter = newValue => { 
    likesCount = newValue; 
    lastCountFromServer = newValue; 
  }

  const undoLike = () => {
    liked = false;
    likesCount = lastCountFromServer;  
    footerMsg = "Oops! Something went wrong. Please try again in a few seconds."
  }

  const onLikeButtonClicked = () => { 
    liked = true; 
    if (typeof likesCount == "number") 
      likesCount += 1;

    incrementLikesCount()
      .then(newValue => {
        updateLikesCounter(newValue);
        footerMsg = "Thank you!"
      })
      .catch(undoLike);
  }

  const onCreateButton = () =>
    getLikesCount().then(updateLikesCounter, () => {})

  return ({
    oncreate: onCreateButton,
    view: () => 
      <table className="like-footer">
        <tr>
          <td className="like-me">
            <LikeCounter liked={liked} likesCount={likesCount}/>
            <LikeButton liked={liked} 
              onLikeButtonClicked={liked ? null : onLikeButtonClicked}/>
          </td>
          <td className="pls-like">
            <em>{footerMsg}</em>
          </td>
        </tr>
      </table>
  })
}

export default app;
