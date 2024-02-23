const path = require('path')

module.exports = {
  entry: {
    home: './src/home.index.js',
    post: './src/post.index.js'
  },
  devServer: {
    contentBase: path.resolve(__dirname, '../static/_site'),
    publicPath: '/js/',
    compress: true,
    port: 9000,
    proxy: {
      '/blogapi': {
        target: "http://localhost:8008",
        headers: { "X-real-ip": "0.0.0.0" },
        pathRewrite: {'^/blogapi' : ''}
      }
    }
  },
  output: {
    path: path.resolve(__dirname, './output/js'),
    filename: '[name].js',
  },
  module: {
    rules: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        use: {
          loader: 'babel-loader',
          options: {
            presets: [
              ['@babel/preset-env', { targets: "defaults" }]
            ],
            plugins: ['@babel/plugin-transform-react-jsx']
          }
        }
      }
    ]
  }
}
