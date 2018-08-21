import m from "mithril";

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
  let likesCount = 0;
  let liked = false;

  const onLikeButtonClicked = () => { 
    likesCount += 1;
    liked = true; 
  }
  return ({
    view: () => 
      <table className="like-footer">
        <tr>
          <td className="like-me">
            <LikeCounter liked={liked} likesCount={likesCount}/>
            <LikeButton liked={liked} 
              onLikeButtonClicked={liked ? null : onLikeButtonClicked}/>
          </td>
          <td className="pls-like">
            <em>If you liked this post, please hit this like button to show your
            appreciation. Thank you!</em>
          </td>
        </tr>
      </table>
  })
}

export default app;
