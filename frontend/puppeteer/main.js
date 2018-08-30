const puppeteer = require('puppeteer');

const { runAPIServer, runDevServer } = require('./server.js')

const main = async () => {
  const [apiServer, devServer, browser] = await Promise.all([ 
    runAPIServer(), 
    runDevServer(), 
    puppeteer.launch()
  ]);

    const page = await browser.newPage();
    await page.goto('http://localhost:9000/posts/2015-12-07-tu-quoque.html')
    await page.screenshot({path: 'example.png'});

    apiServer.kill();
    devServer.kill();
    browser.close()
}

main().then(() => console.log('good'), err => console.log('err', err))
