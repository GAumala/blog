
const currentPath = window.location.pathname;
export const postStringId = currentPath.substr(currentPath.lastIndexOf("/") + 1);
