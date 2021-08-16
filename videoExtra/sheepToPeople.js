
// Reaplce all "sheep" with people on Google Images
for (const node of document.querySelectorAll('.VFACy')) {
	node.textContent = node.textContent.replaceAll('Sheep', 'People').replaceAll('sheep', 'people')
}

// Experiments
'asdf sheep Sheep qwer'.replaceAll('Sheep', 'People').replaceAll('sheep', 'people')
