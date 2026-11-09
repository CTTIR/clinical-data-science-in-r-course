import assert from 'node:assert/strict';
import { createServer } from 'node:http';
import { readFile, mkdir } from 'node:fs/promises';
import { resolve, extname } from 'node:path';
import { chromium } from 'playwright';
const root=resolve('_site');
const types={'.html':'text/html','.js':'application/javascript','.css':'text/css','.json':'application/json','.wasm':'application/wasm','.svg':'image/svg+xml','.png':'image/png'};
let server;
let base=process.env.COURSE_TEST_URL;
if(!base){
 server=createServer(async(req,res)=>{
  try {
   let pathname=decodeURIComponent(new URL(req.url,'http://localhost').pathname).replace(/^\/clinical-data-science-in-r-course\//,'/');
   if(pathname.endsWith('/')) pathname+='index.html';
   const file=resolve(root,'.'+pathname);
   if(!file.startsWith(root+'/')) throw Error('invalid path');
   const body=await readFile(file); res.setHeader('Content-Type',types[extname(file)]||'application/octet-stream');res.end(body);
  }catch {res.statusCode=404;res.end('Not found');}
 });
 await new Promise(r=>server.listen(0,'127.0.0.1',r));
 base=`http://127.0.0.1:${server.address().port}/`;
}
const output=process.env.COURSE_SCREENSHOTS||'/tmp/clinical-course-browser';
await mkdir(output,{recursive:true});
const browser=await chromium.launch({headless:true,...(process.env.CHROMIUM_PATH?{executablePath:process.env.CHROMIUM_PATH}:{})});
const errors=[];
try {
 const page=await browser.newPage({viewport:{width:1440,height:1000}});
 page.on('pageerror',e=>errors.push(e.message));
 page.on('response',r=>{if(r.url().startsWith(base)&&r.status()>=400) errors.push(`${r.status()} ${r.url()}`)});
 await page.goto(base);await page.evaluate(()=>document.fonts.ready);
 assert.equal(await page.locator('.hero h1').innerText(),'Clinical Data Science in R');
 assert.equal(await page.locator('img').evaluateAll(imgs=>imgs.filter(i=>!i.complete||i.naturalWidth===0).length),0);
 await page.screenshot({path:output+'/desktop-home.png',fullPage:true});
 await page.goto(base+'overview.html');
 await page.waitForFunction(()=>document.querySelectorAll('.tutorial-card').length===22);
 assert.equal(await page.locator('#tutorial-network canvas').count(),1);
 await page.screenshot({path:output+'/desktop-network.png',fullPage:true});
 const part=page.locator('#topic-filter-bar button').first();
 await part.click();
 await page.waitForFunction(()=>document.querySelectorAll('.tutorial-card').length===5);
 assert.match(page.url(),/topics=/);
 await page.reload();
 await page.waitForFunction(()=>document.querySelectorAll('.tutorial-card').length===5);
 await page.locator('#filter-reset').click();
 await page.waitForFunction(()=>document.querySelectorAll('.tutorial-card').length===22);
 await page.locator('#overview-search').fill('diagnostic');
 await page.waitForFunction(()=>{const n=document.querySelectorAll('.tutorial-card').length;return n>0&&n<22});
 assert.equal(await page.locator('#search-status').innerText(),'');
 await page.locator('#filter-reset').click();
 await page.locator('#heatmap-wrapper summary').click();
 await page.locator('#heatmap svg').waitFor({state:'visible'});
 await page.evaluate(()=>window.scrollTo(0,0));
 await page.locator('.quarto-color-scheme-toggle').first().click();
 await page.screenshot({path:output+'/desktop-network-alternate-theme.png',fullPage:true});
 await page.locator('.tutorial-card h3 a').first().click();
 assert.match(page.url(),/\/(lessons|exercises)\//);
 await page.locator('h1').waitFor({state:'visible'});
 assert.equal(await page.locator('h1').count(),1);
 await page.setViewportSize({width:390,height:844});
 await page.goto(base+'overview.html');
 await page.waitForFunction(()=>document.querySelectorAll('.tutorial-card').length===22);
 assert.ok(await page.locator('#all-tutorials-nav').isVisible());
 await page.screenshot({path:output+'/mobile-network.png',fullPage:true});
 assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth+1),'Mobile overview overflows');
 await page.screenshot({path:output+'/mobile-network.png',fullPage:true});
 await page.goto(base);
 assert.ok(await page.evaluate(()=>document.documentElement.scrollWidth<=innerWidth+1),'Mobile home overflows');
 await page.screenshot({path:output+'/mobile-home.png',fullPage:true});
 await page.locator('.navbar-toggler').click();
 await page.locator('#navbarCollapse').waitFor({state:'visible'});
 assert.equal(await page.locator('.navbar-toggler').getAttribute('aria-expanded'),'true');
 await page.locator('#navbarCollapse a[href$="overview.html"]').click();
 await page.locator('#tutorial-list .tutorial-card').first().waitFor({state:'visible'});
 assert.deepEqual(errors,[]);
 console.log('Browser checks passed: desktop/mobile, both themes, network, filters, URL restore, search, heatmap, navigation, no broken images or runtime errors.');
}finally{await browser.close();if(server)await new Promise(r=>server.close(r));}
