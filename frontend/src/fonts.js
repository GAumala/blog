export const linkFontFaces = () => {
  const noscriptToReplace = document.getElementById("fontfaces")
  if (noscriptToReplace) {
    const {
      innerText: fontsLinkHTMLString, 
      parentNode: head 
    } = noscriptToReplace

    const newLink = document.createElement("link")
    newLink.innerHTML = fontsLinkHTMLString

    head.replaceChild(newLink, noscriptToReplace);
  }
}
