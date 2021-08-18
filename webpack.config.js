const path = require('path');
const webpack = require('webpack');

module.exports = {
	module: {
		rules: [
			{
				test: /\.html$/,
				type: 'asset/resource'
			}
			,
			{
				test: /\.elm$/,
				exclude: [/elm-stuff/, /node_modules/],

				use: [
					//{ loader: 'elm-hot-webpack-loader' },
					{
						loader: 'elm-webpack-loader',
						options: {
							cwd: __dirname,
							debug: false
						}
					}
				]
			}
		]
	},

	plugins: [
		//new webpack.HotModuleReplacementPlugin()
	],

	mode: 'development',
	devtool: 'source-map',

	devServer: {
		inline: true,
		//hot: true,
		stats: 'errors-only',
		contentBase: './dist'
	}
};