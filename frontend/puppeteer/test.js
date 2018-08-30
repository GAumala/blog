const puppeteer = require('puppeteer');

const { runAPIServer, runDevServer } = require('./server.js')

let apiServer, devServer, browser;

jest.setTimeout(30000);

beforeAll(async () => {
  console.log('setup')
  const services = await Promise.all([ 
    runAPIServer(), 
    runDevServer(), 
    puppeteer.launch() 
  ]);

  apiServer = services[0];
  devServer = services[1];
  browser = services[2];
})

afterAll(() => {
  apiServer.kill();
  devServer.kill();
  browser.close()
})


describe('Blog post page', () => {
  it('is able to post likes', async () => {
    const page = await browser.newPage();
    await page.goto('http://localhost:9000/posts/2015-12-07-tu-quoque.html')
    await page.screenshot({path: 'example.png'});
  })

})

