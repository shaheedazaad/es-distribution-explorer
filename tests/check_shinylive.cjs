// Run with Playwright installed: node tests/check_shinylive.cjs
// PLAYWRIGHT_MODULE may point to an existing Playwright installation.
const { chromium } = require(process.env.PLAYWRIGHT_MODULE || 'playwright');
const assert = require('node:assert/strict');
const { spawn } = require('node:child_process');
const path = require('node:path');
const fs = require('node:fs/promises');
const os = require('node:os');

(async () => {
  const port = 8877;
  const server = spawn('python3', ['-m', 'http.server', String(port), '--bind',
    '127.0.0.1', '--directory', path.resolve(__dirname, '../site')]);
  let browser;
  const artifacts = await fs.mkdtemp(path.join(os.tmpdir(), 'es-explorer-browser-'));
  try {
    await new Promise((resolve, reject) => {
      server.stderr.once('data', reject);
      server.once('error', reject);
      setTimeout(resolve, 1000);
    });
    browser = await chromium.launch({ headless: true });
    const page = await browser.newPage({ viewport: { width: 1440, height: 1000 }, acceptDownloads: true });
    const errors = [];
    page.on('pageerror', error => errors.push(error.message));
    await page.goto(`http://127.0.0.1:${port}`, { waitUntil: 'domcontentloaded' });
    const app = page.frameLocator('iframe');
    await app.locator('#calculate').waitFor({ timeout: 90000 });
    const waitText = (selector, text) => app.locator(selector).filter({ hasText: text }).waitFor({ timeout: 120000 });
    const select = async (id, value) => {
      const control = app.locator(`#${id}`).locator('..').locator('.selectize-control');
      await control.locator('.selectize-input').click();
      await control.locator(`.selectize-dropdown [data-value="${value}"]`).click();
    };
    await app.locator('#keyword').fill('memory');
    await app.locator('#calculate').click();
    await app.locator('.results-heading #explore_distribution').waitFor({ timeout: 120000 });
    assert.equal(await app.locator('#explore_distribution').evaluate(el => getComputedStyle(el).backgroundColor), 'rgb(36, 95, 147)');
    await page.screenshot({ path: path.join(artifacts, 'results.png') });
    await app.locator('#explore_distribution').click();
    await waitText('#explorer_count', 'Rows 1–25');
    assert.equal(await app.locator('#explorer_table tbody tr').count(), 25);
    const initialCount = await app.locator('#explorer_count').innerText();
    await app.locator('#explorer_next').click();
    await waitText('#explorer_count', 'Page 2 of');
    await app.locator('#explorer_previous').click();
    await waitText('#explorer_count', 'Page 1 of');
    await select('explorer_size', '10');
    await waitText('#explorer_count', 'Rows 1–10');
    await select('explorer_sort', 'sample_size');
    await select('explorer_direction', 'desc');
    await page.waitForTimeout(750);
    const values = await app.locator('#explorer_table tbody tr td:nth-child(8)').allTextContents();
    const numbers = values.map(Number);
    assert(numbers.every((n, i) => !i || numbers[i - 1] >= n), 'Sample sizes sorted numerically descending');
    await app.locator('#explorer_search').fill('Cognition');
    await page.waitForTimeout(750);
    const count = await app.locator('#explorer_count').innerText();
    const total = Number(count.match(/of (\d+) ·/)[1]);
    assert(total > 10, 'CSV test spans multiple pages');
    assert.notEqual(count, initialCount);
    const downloadPromise = page.waitForEvent('download');
    await app.locator('#download_effects').click();
    const download = await downloadPromise;
    const csv = path.join(artifacts, download.suggestedFilename());
    await download.saveAs(csv);
    const { execFileSync } = require('node:child_process');
    const csvCount = Number(execFileSync('python3', ['-c',
      'import csv,sys; print(len(list(csv.DictReader(open(sys.argv[1])))))', csv], { encoding: 'utf8' }).trim());
    assert.equal(csvCount, total, 'Download includes every matching row');
    await page.screenshot({ path: path.join(artifacts, 'explorer.png') });
    await app.locator('#explorer_search').fill('no-such-keyword-12345');
    await waitText('#explorer_count', 'Rows 0–0 of 0');
    await app.getByRole('button', { name: 'Close', exact: true }).click();
    await app.locator('#keyword').fill('no-such-keyword-12345');
    await app.locator('#explore_distribution').click();
    await waitText('#explorer_count', initialCount);
    await app.getByRole('button', { name: 'Close', exact: true }).click();
    console.log('PASS: distribution card placement, highlight, pagination, numeric sorting, search, CSV, empty results, and completed-filter snapshot');

    await app.getByRole('tab', { name: 'Z-curve', exact: true }).click();
    await app.locator('#keyword').fill('memory');
    await app.locator('#calculate').click();
    await app.locator('.zcurve-heading #explore_zcurve').waitFor();
    await app.locator('#zcurve_bootstraps').fill('1');
    await app.locator('#fit_zcurve').click();
    await waitText('#zcurve_estimates', 'significant effects');
    await app.locator('#explore_zcurve').click();
    await waitText('#explorer_count', initialCount);
    await app.getByRole('button', { name: 'Close', exact: true }).click();
    await page.setViewportSize({ width: 390, height: 844 });
    await app.locator('#explore_zcurve').click();
    await waitText('#explorer_count', 'Rows 1–25');
    // Let Bootstrap finish the modal's fade/slide before checking its layout.
    await page.waitForTimeout(400);
    await app.locator('.modal').evaluate(el => { el.scrollTop = 0; });
    await page.screenshot({ path: path.join(artifacts, 'mobile-explorer.png') });
    assert.equal(await app.locator('.modal-dialog').evaluate(el => el.getBoundingClientRect().width <= innerWidth), true);
    assert.deepEqual(errors, []);
    console.log('PASS: completed z-curve explorer and mobile modal; no browser page errors');
    console.log(`Artifacts: ${artifacts}`);
  } finally {
    if (browser) await browser.close();
    server.kill();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
