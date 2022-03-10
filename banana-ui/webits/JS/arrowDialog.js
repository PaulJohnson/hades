/*
Copyright © Paul Johnson 2021. Licensed under BSD3 terms.
*/

function fancyToTree (r) {return ([r.key,fancyToForest(r.getChildren())])};

function fancyToForest (rs) {return(Array.isArray(rs)?rs.map(fancyToTree):[])}

function openTab(btn,target,hc,tc) {
  var i,cs,tablinks;
  cs=hc.children;
  for(i=0;i<cs.length;i++){cs[i].classList.remove("active");}
  btn.parentNode.classList.add("active");
  cs=tc.children;
  for(i=0;i<cs.length;i++){cs[i].style.display="none";}
  document.getElementById(target).style.display="block"
}


// Find the ancestral zIndex of the argument element in the DOM. 0 if none found.
function ancestorZ (elem) {
  let ez=elem.style.zIndex;
  let ezi=parseInt(ez,10);
  if (ez=="initial") {
    return(0);
  } else if (!isNaN(ezi)) {
    return(ezi);
  } else if (Number.isInteger(ez)) {
    return(ez);
  } else if (elem.parentElement) {
    return (ancestorZ (elem.parentElement));
  } else {
    return(0);
  }
}


function legalDrop (node, data) {
  if (data.otherNode.tree !== data.tree) {return (false);}
  if (node.data.legalParent) {return (true)}
  return ['before', 'after'];
}



// Hide all popup menus when anything else is clicked..
$(document).on("mousedown", function (e) {
  if (!$(e.target).parents(".popup-menu").length > 0) {
    $(".popup-menu").hide(100);
  }
});
