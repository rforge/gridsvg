function showTooltip(evt, label) {
  var svgNS = "http://www.w3.org/2000/svg";

  var target = evt.currentTarget;
  
  // Create new text node, rect and text for the tooltip
  var content = document.createTextNode(label);  

  var text = document.createElementNS(svgNS, "text");
  text.setAttribute("id", "tooltipText");
  // Resetting some style attributes
  text.setAttribute("font-size", "16px");
  text.setAttribute("fill", "black");
  text.setAttribute("stroke-width", "0");
  text.appendChild(content);

  var rect = document.createElementNS(svgNS, "rect");
  rect.setAttribute("id", "tooltipRect");

  // Add rect and text to the parent of the element
  var targetParent = target.parentNode;
  targetParent.appendChild(rect);
  targetParent.appendChild(text);

  // Determine position for tooltip based on location of 
  // element that mouse is over
  // AND size of text label
  var tooltipx = target.getBBox().x + target.getBBox().width;
  var tooltiplabx = tooltipx + 5;
  var tooltipy = target.getBBox().y + target.getBBox().height;
  var tooltiplaby = tooltipy + 5;

  // Position tooltip rect and text
  text.setAttribute("transform", 
                    "translate(" + tooltiplabx + ", " + tooltiplaby + ") " +
                    "scale(1, -1)");

  rect.setAttribute("x", tooltipx);
  rect.setAttribute("y", tooltipy);
  rect.setAttribute("width", text.getBBox().width + 10);
  rect.setAttribute("height", text.getBBox().height + 5);
  rect.setAttribute("stroke", "black");
  rect.setAttribute("fill", "yellow");
}

function hideTooltip() {
  // Remove tooltip text and rect
  var text = document.getElementById("tooltipText");
  text.parentNode.removeChild(text);
  var rect = document.getElementById("tooltipRect");
  rect.parentNode.removeChild(rect);  
}
