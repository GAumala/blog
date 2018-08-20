var heart = document.getElementById("heart");
var likeCounter = document.getElementById("like-count");
var postLikesCount = 0;
var liked = false;

heart.addEventListener("click", function () {
  if (! liked) {
    postLikesCount += 1;
    likeCounter.innerHTML = "<strong>" + postLikesCount + "</strong>";
    likeCounter.className = "liked";

    heart.className = "heartAnimation";
    heart.style.backgroundPosition = "right"
    liked = true
  }
});
