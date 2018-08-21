const path = require('path')

module.exports = {
  entry: './src/index.js',
  devServer: {
    contentBase: path.resolve(__dirname, '../static/_site'),
    publicPath: '/js/',
    compress: true,
    port: 9000,
    proxy: {
      '/blogapi': {
        target: "http://localhost:8008",
        pathRewrite: {'^/blogapi' : ''}
      }
    }
  },
  output: {
    path: path.resolve(__dirname, './output/js'),
    filename: 'like.js',
  },
  module: {
    rules: [{
      test: /\.js$/,
      exclude: /node_modules/,
      loader: 'babel-loader'
    }]
  }
}
