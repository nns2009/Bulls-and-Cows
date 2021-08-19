'use strict';

import './launchElm'

// It doesn't work as expected: Elm state is lost and something gets broken
// if (module.hot) {
// 	module.hot.accept('./launchElm', () => {});
// }