var heart = document.getElementById("heart");
var likeCounter = document.getElementById("like-count");
var postLikesCount = 0;
var liked = false;
var currentPostId = window.location.pathname.substr("/posts/".length)

function redrawLikesCounter() {
  likeCounter.innerText = postLikesCount
}

function getPostLikes(postStringId) {
  var req = XMLHttpRequest();
  req.open("GET", "/blogapi/likes/" + postStringId, true)
  req.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      postLikesCount = this.responseText;
      redrawLikesCounter();
    }
  }
  req.send();
}

function likePost(postStringId) {
  var req = XMLHttpRequest();
  req.open("POST", "/blogapi/like/" + postStringId, true)
  req.onreadystatechange = function() {
    if (this.readyState == 4 && this.status == 200) {
      postLikesCount = this.responseText;
      redrawLikesCounter();
    }
  }
  req.send();
}

heart.addEventListener("click", function () {
  if (! liked) {
    postLikesCount += 1;
    likeCounter.className = "liked";
    redrawLikesCounter();

    heart.className = "heartAnimation";
    heart.style.backgroundPosition = "right";
    liked = true;
  }
});
