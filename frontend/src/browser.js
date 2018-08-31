
const currentPath = window.location.pathname;
const idStart = currentPath.lastIndexOf("/") + 1;
const idEnd = currentPath.lastIndexOf(".");
export const postStringId = currentPath.substring(idStart, idEnd);
