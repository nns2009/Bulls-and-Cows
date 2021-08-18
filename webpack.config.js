const path = require('path');
const webpack = require('webpack');
const { CleanWebpackPlugin } = require("clean-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");

let mode = "development";
if (process.env.NODE_ENV === 'production') {
	mode = 'production';
}

module.exports = {
	mode,

	output: {
		path: path.resolve(__dirname, 'dist'),
	},

	module: {
		rules: [
			{
				test: /\.elm$/,
				exclude: [/elm-stuff/, /node_modules/],

				use: [
					{ loader: 'elm-hot-webpack-loader' },
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
		new CleanWebpackPlugin(),
		new HtmlWebpackPlugin({
			template: './src/index.html',
		}),
		//new webpack.HotModuleReplacementPlugin(),
	],

	devtool: 'source-map',

	devServer: {
		inline: true,
		hot: true,
		stats: 'errors-only',
		contentBase: './dist'
	}
};