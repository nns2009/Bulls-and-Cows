const sheep2people =
	s => s.replaceAll('Sheep', 'People').replaceAll('sheep', 'people');

document.title = sheep2people(document.title);
// Replace all "sheep" with people on Google Images titles
for (const node of document.querySelectorAll('.VFACy')) {
	node.textContent = sheep2people(node.textContent);
}

// Experiments
'asdf sheep Sheep qwer'.replaceAll('Sheep', 'People').replaceAll('sheep', 'people')
