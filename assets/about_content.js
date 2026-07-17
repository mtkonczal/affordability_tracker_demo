// The About / sources-and-methods content — single source of truth, loaded
// as a script payload (like the data/*.js files) by BOTH about.html (the
// standalone page) and index.html (the in-app About view). A plain <script>
// include works everywhere, including file:// where fetch() is blocked.
//
// Edit the HTML below directly; it's a JS template literal, so avoid
// backticks (`) and ${ inside the content.
window.ABOUT_CONTENT = `
    <h2><span class="sec">§01</span> How the tracker updates</h2>
    <p class="lead">
      The time-series data refreshes automatically on the <b>1st of each month</b>,
      with manual refreshes as needed after major federal releases. If any source fails
      to fetch, the site keeps serving the last good data rather than a partial update. The annual state indicators (childcare, health
      premiums, debt burdens) are updated once a year when their sources publish.
      Every chart offers a CSV of the exact series shown and a shareable PNG.
    </p>

    <h2><span class="sec">§02</span> National series</h2>
    <div class="table-wrap">
    <table>
      <thead><tr><th>Series</th><th>Source</th><th>Updates</th><th>Notes</th></tr></thead>
      <tbody>
        <tr>
          <td><b>Groceries, electricity, rent (CPI), cars, car insurance, health insurance, childcare, inflation</b></td>
          <td>BLS Consumer Price Index, via FRED and the BLS public API</td>
          <td><span class="freq m">Monthly</span></td>
          <td>Seasonally adjusted where published. Index series display as cumulative % change, never raw index points. Each card links to its FRED series ID.</td>
        </tr>
        <tr>
          <td><b>Eggs, ground beef, chicken, milk, bread, bananas, potatoes</b></td>
          <td>BLS Average Price data (US city average)</td>
          <td><span class="freq m">Monthly</span></td>
          <td>Actual retail dollar prices, not indexes.</td>
        </tr>
        <tr>
          <td><b>Gasoline</b></td>
          <td>US Energy Information Administration, via FRED</td>
          <td><span class="freq w">Weekly</span></td>
          <td>Regular unleaded, US average pump price.</td>
        </tr>
        <tr>
          <td><b>Water &amp; sewer</b></td>
          <td>BLS CPI (water, sewer &amp; trash collection services), via FRED</td>
          <td><span class="freq m">Monthly</span></td>
          <td>National only. No public state-level water-rate series exists (the AWWA rate survey is proprietary), so the tracker does not show state water bills.</td>
        </tr>
        <tr>
          <td><b>Rent (market)</b></td>
          <td>Zillow Observed Rent Index (ZORI)</td>
          <td><span class="freq m">Monthly</span></td>
          <td>Typical <i>asking</i> rent for a new lease, in dollars — a different concept from CPI rent, which tracks all tenants including long-standing leases. Smoothed, seasonally adjusted.</td>
        </tr>
        <tr>
          <td><b>Median home price</b></td>
          <td>Census Bureau / HUD, via FRED</td>
          <td><span class="freq q">Quarterly</span></td>
          <td>Median sales price of houses sold.</td>
        </tr>
        <tr>
          <td><b>30-year mortgage rate</b></td>
          <td>Freddie Mac Primary Mortgage Market Survey, via FRED</td>
          <td><span class="freq w">Weekly</span></td>
          <td></td>
        </tr>
        <tr>
          <td><b>Wages, unemployment, job openings, quits, unemployment duration</b></td>
          <td>BLS (CES, CPS, JOLTS), via FRED</td>
          <td><span class="freq m">Monthly</span></td>
          <td>Real hourly earnings are derived: nominal earnings deflated by CPI-U, expressed in the latest month's dollars.</td>
        </tr>
        <tr>
          <td><b>Credit card, student loan &amp; auto debt; delinquency rates</b></td>
          <td>Federal Reserve (G.19 release; bank call reports), via FRED</td>
          <td><span class="freq m">Monthly</span> <span class="freq q">Quarterly</span></td>
          <td>Balances owned and securitized; delinquency is 30+ days at all commercial banks.</td>
        </tr>
        <tr>
          <td><b>ACA benchmark premium</b></td>
          <td><a href="https://www.kff.org/affordable-care-act/state-indicator/average-marketplace-premiums-by-metal-tier/" target="_blank" rel="noopener">KFF</a> State Health Facts (Average Marketplace Premiums by Metal Tier)</td>
          <td><span class="freq a">Annual</span></td>
          <td>Monthly premium for the second-lowest-cost silver ("benchmark") plan for a 40-year-old — the plan ACA subsidies are pegged to. Plan years 2018 onward.</td>
        </tr>
        <tr>
          <td><b>Uninsured rate</b></td>
          <td><a href="https://www.kff.org/state-health-policy-data/state-indicator/total-population/" target="_blank" rel="noopener">KFF</a> State Health Facts (Health Insurance Coverage of the Total Population)</td>
          <td><span class="freq a">Annual</span></td>
          <td>Share of the total population with no health coverage, KFF estimates from the Census ACS. 2008 onward; no 2020 point (the ACS did not release comparably that year).</td>
        </tr>
        <tr>
          <td><b>Household debt per capita</b></td>
          <td><a href="https://www.newyorkfed.org/microeconomics/databank.html" target="_blank" rel="noopener">NY Fed</a> Consumer Credit Panel / Equifax, State-Level Household Debt Statistics</td>
          <td><span class="freq a">Annual</span></td>
          <td>Total household debt per person with a credit file; each point is Q4, 2003 onward. The same panel supplies the state metrics, so national and state lines are directly comparable.</td>
        </tr>
        <tr>
          <td><b>Rent burden</b></td>
          <td>Census ACS 1-year estimates, table B25070</td>
          <td><span class="freq a">Annual</span></td>
          <td>Share of renter households paying 30%+ of income on gross rent. 2005 onward; no 2020 point. Households where the ratio cannot be computed are excluded.</td>
        </tr>
        <tr>
          <td><b>Income: 20th &amp; 80th percentile</b></td>
          <td>Census ACS 1-year estimates, table B19080</td>
          <td><span class="freq a">Annual</span></td>
          <td>Quintile upper limits: the income levels 20% and 80% of households fall below. The ACS publishes quintile limits, not arbitrary percentiles — 20th/80th are the exact published points, so the tracker uses them rather than interpolating a 25th/75th. Nominal dollars (the Real toggle deflates them); median income remains inflation-adjusted as published.</td>
        </tr>
        <tr>
          <td><b>Rent in hours of work</b></td>
          <td>Derived: Zillow ZORI ÷ BLS average hourly earnings</td>
          <td><span class="freq m">Monthly</span></td>
          <td>Hours at the average private-sector wage needed to pay one month's market-rate rent. 2015 onward.</td>
        </tr>
      </tbody>
    </table>
    </div>

    <h2><span class="sec">§03</span> State series</h2>
    <p class="lead">
      Every state series covers all 50 states plus DC unless noted. The state view shows
      every series for one state (the US is available as an option); the compare view puts
      any set of states — including the US — on one chart, and the map view shades all
      states by one measure at a time.
    </p>
    <div class="table-wrap">
    <table>
      <thead><tr><th>Series</th><th>Source</th><th>Updates</th><th>Notes</th></tr></thead>
      <tbody>
        <tr>
          <td><b>Unemployment rate</b></td>
          <td>BLS Local Area Unemployment Statistics, via FRED</td>
          <td><span class="freq m">Monthly</span></td>
          <td>Seasonally adjusted.</td>
        </tr>
        <tr>
          <td><b>Hourly wages</b></td>
          <td>BLS state Current Employment Statistics, via FRED</td>
          <td><span class="freq m">Monthly</span></td>
          <td>Average hourly earnings, all private employees. Not seasonally adjusted; series begin in 2007.</td>
        </tr>
        <tr>
          <td><b>Home prices</b></td>
          <td>FHFA All-Transactions House Price Index, via FRED</td>
          <td><span class="freq q">Quarterly</span></td>
          <td>An index (displayed as % change), not a dollar price — dollar-level state sale prices require proprietary data.</td>
        </tr>
        <tr>
          <td><b>Median household income</b></td>
          <td>Census Bureau (CPS ASEC), via FRED</td>
          <td><span class="freq a">Annual</span></td>
          <td>Real (inflation-adjusted) dollars.</td>
        </tr>
        <tr>
          <td><b>Rent (market)</b></td>
          <td>Zillow ZORI, county data aggregated by ESP</td>
          <td><span class="freq m">Monthly</span></td>
          <td>See methods below — Zillow doesn't publish state ZORI, so we aggregate its county index with renter-household weights. States where covered counties hold under half of renter households are omitted; each state card shows its coverage.</td>
        </tr>
        <tr>
          <td><b>Electricity bills</b></td>
          <td>EIA Form 861M</td>
          <td><span class="freq m">Monthly</span></td>
          <td>Average monthly residential bill (revenue ÷ customers). Appears once the tracker's EIA API key is configured; recent months are preliminary and revised.</td>
        </tr>
        <tr>
          <td><b>ACA benchmark premium</b></td>
          <td><a href="https://www.kff.org/affordable-care-act/state-indicator/average-marketplace-premiums-by-metal-tier/" target="_blank" rel="noopener">KFF</a> State Health Facts</td>
          <td><span class="freq a">Annual</span></td>
          <td>Same benchmark (second-lowest silver, age 40) premium as the national card, per state, plan years 2018 onward. No state-level price index exists — this is a dollar premium, not a CPI.</td>
        </tr>
        <tr>
          <td><b>Uninsured rate</b></td>
          <td><a href="https://www.kff.org/state-health-policy-data/state-indicator/total-population/" target="_blank" rel="noopener">KFF</a> State Health Facts</td>
          <td><span class="freq a">Annual</span></td>
          <td>Share of each state's population with no health coverage, KFF estimates from the Census ACS. 2008 onward, no 2020 point.</td>
        </tr>
        <tr>
          <td><b>Household debt, student loan debt (per capita); credit card delinquency (90+)</b></td>
          <td><a href="https://www.newyorkfed.org/microeconomics/databank.html" target="_blank" rel="noopener">NY Fed</a> Consumer Credit Panel / Equifax, State-Level Household Debt Statistics</td>
          <td><span class="freq a">Annual</span></td>
          <td>Per person with a credit file (ages 18+), not per resident; delinquency is percent of balance 90+ days late — stricter than the 30-day national bank series. Each point is Q4, 2003 onward. One panel covers the US and every state, so cross-state and national comparisons stay apples-to-apples.</td>
        </tr>
        <tr>
          <td><b>Rent burden</b></td>
          <td>Census ACS 1-year, table B25070</td>
          <td><span class="freq a">Annual</span></td>
          <td>Share of renter households paying 30%+ of income on gross rent. 2005 onward, no 2020 point.</td>
        </tr>
        <tr>
          <td><b>Income: 20th &amp; 80th percentile</b></td>
          <td>Census ACS 1-year, table B19080</td>
          <td><span class="freq a">Annual</span></td>
          <td>Quintile upper limits — the exact percentiles the ACS publishes (see the national table's note). Nominal dollars.</td>
        </tr>
        <tr>
          <td><b>Rent in hours of work</b></td>
          <td>Derived: Zillow ZORI ÷ BLS state hourly earnings</td>
          <td><span class="freq m">Monthly</span></td>
          <td>Hours at the state's average private-sector wage to pay a month's market rent. Inherits the rent series' coverage caveats.</td>
        </tr>
      </tbody>
    </table>
    </div>

    <p class="lead" style="margin-top:14px">
      <b>Rankings.</b> Every state metric carries a national rank computed at data-build
      time from the latest value (1 = highest, ties share a rank). Ranks are
      direction-neutral — "1st highest" income is good news and "1st highest" rent is not —
      and cover only states with data (market rent omits low-coverage states, for example),
      so the denominator is shown alongside every rank.
    </p>

    <h2><span class="sec">§04</span> Annual state indicators</h2>
    <p class="lead">
      Some of the costs that matter most are only published once a year. These appear as
      stat tiles on each state's page — context for the live series, not live tracking.
    </p>
    <div class="table-wrap">
    <table>
      <thead><tr><th>Indicator</th><th>Source &amp; vintage</th><th>Definition</th></tr></thead>
      <tbody>
        <tr>
          <td><b>Infant child care, center-based</b></td>
          <td><a href="https://info.childcareaware.org/price-and-supply-2025" target="_blank" rel="noopener">Child Care Aware of America</a>, 2025</td>
          <td>Annual price of full-time center-based infant care. CO, DC, NM and SC did not report a 2025 price. Seven states (AL, FL, MT, PA, TX, WV, WY) carry a footnote flag in CCAoA's published tables — check the report before quoting those states specifically.</td>
        </tr>
        <tr>
          <td><b>Medical debt in collections</b></td>
          <td><a href="https://apps.urban.org/features/debt-interactive-map/" target="_blank" rel="noopener">Urban Institute, Debt in America</a>, Aug 2025 panel</td>
          <td>Share of people with a credit record who have medical debt in collections. Seven states that restrict medical-debt credit reporting (CA, CO, IL, NY, RI, VT, WA) have no comparable 2025 value and show no tile — an artifact of reporting rules, not zero medical debt.</td>
        </tr>
        <tr>
          <td><b>Any debt in collections</b></td>
          <td>Urban Institute, Debt in America, Aug 2025 panel</td>
          <td>Share of people with a credit record with any debt in collections.</td>
        </tr>
      </tbody>
    </table>
    </div>

    <h2><span class="sec">§05</span> Methods, honestly stated</h2>

    <div class="method">
      <h3>The three anchors</h3>
      <p>The tracker measures change from three moments: <b>January 2025</b> (the current
      administration), <b>2019</b> (just before the pandemic), and <b>2000</b>
      (a generation). The 2019 anchor is December 2019, used deliberately rather than early
      2020: anchoring at 2020 bakes the COVID crash into the baseline — gas prices collapsed
      in April 2020, for instance — and produces comparisons that don't survive scrutiny.
      The tracker opens on the 2019 anchor by default.</p>
    </div>

    <div class="method">
      <h3>Nominal vs. real dollars</h3>
      <p>A global <b>Nominal / Real</b> switch controls the whole page. <b>Nominal</b> is what
      you actually pay at the register. <b>Real</b> deflates each dollar and price-index series
      by the CPI-U (all items) and re-expresses it in the latest month's dollars, so you see
      whether something rose faster or slower than inflation overall — the newest point is
      unchanged and earlier points are lifted into today's dollars. Real is a no-op for rates
      (unemployment, mortgage rate, uninsured rate), durations, and counts, which are already
      normalized over time, and for series that arrive already inflation-adjusted (median
      household income, real hourly earnings), which are never deflated twice. Because no
      state-level price index exists, state series are deflated by the national CPI-U — the
      standard fallback.</p>
    </div>

    <div class="method">
      <h3>Indexes display as percent change</h3>
      <p>CPI and house-price indexes have arbitrary base years (a CPI of 320 means nothing
      by itself), so the tracker never shows raw index points. Index series are rebased to
      cumulative percent change from the start of whatever window you've selected. Dollar
      series ($ per dozen, $ per month) display in dollars, with a global toggle to view
      everything as percent change for comparison.</p>
    </div>

    <div class="method">
      <h3>No state CPI exists</h3>
      <p>The BLS does not publish state-level consumer price indexes — anyone showing you
      "grocery inflation in Ohio" is estimating with proprietary data. This tracker shows
      only what genuinely exists at the state level: unemployment, wages, home prices,
      incomes, market rents, and electricity bills, plus the annual indicators above.
      Grocery and insurance prices are national.</p>
    </div>

    <div class="method">
      <h3>How state rents are built</h3>
      <p>Zillow publishes its Observed Rent Index for counties but not states. We aggregate
      county ZORI to states using fixed county renter-household weights (Census ACS
      2019–2023), the appropriate universe for a rental price measure. States where the
      covered counties hold less than half of the state's renter households are omitted
      rather than shown thin, and every state's coverage share is displayed on its card.
      ZORI measures <i>asking</i> rent on new leases — it moves earlier and faster than
      CPI rent, which averages over all tenants.</p>
    </div>

    <div class="method">
      <h3>Real wages</h3>
      <p>Real hourly earnings deflate average hourly earnings (BLS CES) by CPI-U (all
      items, seasonally adjusted) and express the result in the most recent month's
      dollars, so the latest point matches the nominal series and history shows what past
      paychecks would buy today. This is the standard construction; sources are printed on
      the card.</p>
    </div>

    <div class="method">
      <h3>Mixing seasonally adjusted and unadjusted series</h3>
      <p>National CPI series are seasonally adjusted where the BLS publishes them that
      way. State wage series (CES) and the FHFA house price index are not seasonally
      adjusted — comparisons across states at the same date are clean, but month-to-month
      wiggles within a state partly reflect seasonality. Year-over-year and anchor-based
      changes, which the tracker leads with, are robust to this.</p>
    </div>

    <p class="note">
      Corrections and methods questions: the underlying pipeline is a single R script that
      declares every series identifier explicitly; every card's CSV download embeds its
      source URL. If a number here can't be traced to a public series in two clicks,
      that's a bug — tell us.
    </p>
  
`;
