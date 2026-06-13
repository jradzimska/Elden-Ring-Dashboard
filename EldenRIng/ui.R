options(shiny.autoload.r = FALSE)
options(sass.cache = FALSE)

library(shiny)
library(DT)
library(bslib)
library(plotly)

elden_css <- "
@import url('https://fonts.googleapis.com/css2?family=Cinzel:wght@400;600;700;900&family=Cinzel+Decorative:wght@400;700&family=Crimson+Text:ital,wght@0,400;0,600;1,400&display=swap');
:root {
  --gold:    #C9A84C;
  --gold-lt: #F0D080;
  --gold-dk: #7A5A10;
  --bg:      #0A0A0A;
  --panel:   #111111;
  --border:  #2A2218;
  --text:    #D4C5A0;
  --text-dim:#8A7B60;
}
body, html {
  background-color: var(--bg) !important;
  color: var(--text) !important;
  font-family: 'Crimson Text', serif !important;
  font-size: 16px;
}
.navbar { display: none !important; }
#header-banner {
  background: linear-gradient(180deg, #000 0%, #0d0b07 60%, #1a1408 100%);
  border-bottom: 2px solid var(--gold-dk);
  padding: 18px 32px 14px;
  display: flex;
  align-items: center;
  gap: 22px;
  position: relative;
}
#header-banner::after {
  content: '';
  position: absolute;
  bottom: 0; left: 0; right: 0;
  height: 1px;
  background: linear-gradient(90deg, transparent, var(--gold), transparent);
}
.er-monogram {
  width: 64px; height: 64px;
  border: 2px solid var(--gold);
  border-radius: 50%;
  display: flex; align-items: center; justify-content: center;
  font-family: 'Cinzel Decorative', serif;
  font-size: 22px;
  font-weight: 700;
  color: var(--gold);
  background: radial-gradient(circle, #1a1408 0%, #000 100%);
  box-shadow: 0 0 18px rgba(201,168,76,0.3), inset 0 0 12px rgba(0,0,0,0.6);
  flex-shrink: 0;
}
.er-title-block h1 {
  font-family: 'Cinzel', serif !important;
  font-size: 26px !important;
  font-weight: 700;
  color: var(--gold-lt) !important;
  letter-spacing: 3px;
  text-transform: uppercase;
  margin: 0 0 2px 0;
  text-shadow: 0 0 20px rgba(201,168,76,0.4);
}
.er-title-block p {
  font-family: 'Crimson Text', serif;
  font-style: italic;
  color: var(--text-dim);
  margin: 0;
  font-size: 14px;
  letter-spacing: 1px;
}
.nav-pills { gap: 4px !important; flex-wrap: wrap !important; }
.nav-pills .nav-link,
.nav-pills > li > a,
ul.nav.nav-pills li a {
  font-family: 'Cinzel', serif !important;
  font-size: 12px !important;
  letter-spacing: 1.5px !important;
  text-transform: uppercase !important;
  border-radius: 2px !important;
  padding: 7px 0 !important;
  min-width: 110px !important;
  width: 110px !important;
  text-align: center !important;
  display: inline-block !important;
  box-sizing: border-box !important;
  cursor: pointer !important;
  color: #C9A84C !important;
  background-color: #0A0A0A !important;
  background-image: none !important;
  border: 1px solid #7A5A10 !important;
}
.nav-pills .nav-link.active,
.nav-pills > li.active > a,
ul.nav.nav-pills li.active a {
  color: #0A0A0A !important;
  background-color: #C9A84C !important;
  background-image: none !important;
  border: 1px solid #C9A84C !important;
  font-weight: 700 !important;
  box-shadow: 0 0 10px rgba(201,168,76,0.4) !important;
}
.nav-pills .nav-link:hover,
.nav-pills > li > a:hover {
  color: #F0D080 !important;
  background-color: #1a1408 !important;
  border-color: #C9A84C !important;
}
/* hover handled via JS inline styles too - see erStyleTabs mouseenter/leave */
.sidebar-panel {
  background: var(--panel) !important;
  border: 1px solid var(--border) !important;
  border-radius: 3px;
  padding: 16px !important;
}
.sidebar-panel h4 {
  font-family: 'Cinzel', serif;
  font-size: 13px;
  letter-spacing: 2px;
  color: var(--gold);
  text-transform: uppercase;
  border-bottom: 1px solid var(--border);
  padding-bottom: 8px;
  margin-bottom: 14px;
}
.sidebar-panel hr { border-color: var(--border); margin: 10px 0; }
.irs--shiny .irs-bar { background: var(--gold) !important; border-color: var(--gold-dk) !important; }
.irs--shiny .irs-handle { background: var(--gold-lt) !important; border-color: var(--gold) !important; }
.irs--shiny .irs-single { background: var(--gold-dk) !important; color: #fff !important; }
.irs--shiny .irs-from, .irs--shiny .irs-to { background: var(--gold-dk) !important; color: #fff !important; }
.irs--shiny .irs-from::before, .irs--shiny .irs-to::before { border-top-color: var(--gold-dk) !important; }
.irs--shiny .irs-line { background: #2a2218 !important; }
.irs-min, .irs-max { color: var(--text-dim) !important; }
label { color: var(--text-dim) !important; font-size: 13px; font-family: 'Cinzel', serif; letter-spacing: 0.5px; }
.selectize-input, .selectize-dropdown {
  background: #1a1408 !important;
  color: var(--text) !important;
  border: 1px solid var(--border) !important;
  border-radius: 2px !important;
  box-shadow: none !important;
}
.selectize-dropdown-content .option:hover { background: var(--gold-dk) !important; }
.selectize-dropdown-content .selected { background: var(--gold-dk) !important; }
.er-card {
  background: var(--panel);
  border: 1px solid var(--border);
  border-radius: 3px;
  padding: 16px;
  margin-bottom: 14px;
}
.er-card-title {
  font-family: 'Cinzel', serif;
  font-size: 12px;
  letter-spacing: 2px;
  color: var(--gold);
  text-transform: uppercase;
  margin-bottom: 10px;
  padding-bottom: 8px;
  border-bottom: 1px solid var(--border);
}
.dataTables_wrapper,
table.dataTable thead th,
table.dataTable tbody td {
  color: var(--text) !important;
  background: transparent !important;
  border-color: var(--border) !important;
  font-family: 'Crimson Text', serif !important;
  font-size: 15px;
}
table.dataTable thead th {
  font-family: 'Cinzel', serif !important;
  font-size: 11px !important;
  letter-spacing: 1.5px;
  text-transform: uppercase;
  color: var(--gold) !important;
  border-bottom: 1px solid var(--gold-dk) !important;
}
table.dataTable tbody tr:hover td { background: #1a1408 !important; }
table.dataTable tbody tr:not(.selected):hover td {
  background-color: #2a1e08 !important;
  color: var(--gold-lt) !important;
  cursor: pointer !important;
}
table.dataTable tbody tr.selected td,
table.dataTable tbody tr.selected td.sorting_1,
table.dataTable tbody tr.selected td.sorting_2,
table.dataTable tbody tr.selected td.sorting_3,
table.dataTable tbody > tr.selected > td,
table.dataTable tbody > tr > td.selected,
.dataTables_wrapper tbody tr.selected td,
.dataTables_scrollBody tbody tr.selected td,
table.dataTable tbody tr.odd.selected > td,
table.dataTable tbody tr.even.selected > td,
table.dataTable tbody tr.selected > td.sorting_1,
table.dataTable tbody tr.selected > td.sorting_2,
table.dataTable tbody tr.selected > td.sorting_3 {
  background-color: #C9A84C !important;
  background: #C9A84C !important;
  color: #0A0A0A !important;
  font-weight: 600 !important;
  box-shadow: inset 0 0 0 9999px #C9A84C !important;
}
.dataTables_filter input,
.dataTables_length select {
  background: #1a1408 !important;
  color: var(--text) !important;
  border: 1px solid var(--border) !important;
  border-radius: 2px;
}
.dataTables_info, .dataTables_paginate, .dataTables_filter label, .dataTables_length label {
  color: var(--text-dim) !important;
  font-family: 'Cinzel', serif !important;
  font-size: 11px !important;
}
.paginate_button { color: var(--text-dim) !important; }
.paginate_button.current { background: var(--gold-dk) !important; color: var(--gold-lt) !important; border-color: var(--gold-dk) !important; }
.paginate_button:hover { background: #1a1408 !important; color: var(--gold) !important; }
.stat-row { display: flex; align-items: center; gap: 10px; margin-bottom: 7px; }
.stat-label { font-family: 'Cinzel', serif; font-size: 11px; letter-spacing: 1px; color: var(--text-dim); width: 38px; flex-shrink: 0; }
.stat-bar-bg { flex: 1; height: 8px; background: #1a1408; border-radius: 2px; overflow: hidden; border: 1px solid var(--border); }
.stat-bar-fill { height: 100%; border-radius: 2px; transition: width 0.4s ease; }
.stat-bar-str { background: linear-gradient(90deg, #8B4513, #D2691E); }
.stat-bar-dex { background: linear-gradient(90deg, #4B6B2E, #7DBA4B); }
.stat-bar-int { background: linear-gradient(90deg, #1A3A6A, #4A8FD4); }
.stat-bar-fai { background: linear-gradient(90deg, #5A4A8A, #9A7ACA); }
.stat-bar-arc { background: linear-gradient(90deg, #6A2A5A, #CA5A9A); }
.stat-value { font-family: 'Cinzel', serif; font-size: 12px; color: var(--gold-lt); width: 30px; text-align: right; flex-shrink: 0; }
#weapon-detail-panel {
  background: var(--panel);
  border-radius: 3px;
  padding: 4px 0 0 0;
  min-height: 120px;
}
.weapon-detail-inner { display: flex; gap: 16px; align-items: flex-start; }
.weapon-detail-left { flex-shrink: 0; width: 90px; text-align: center; }
.weapon-detail-right { flex: 1; min-width: 0; }
.weapon-img-frame {
  width: 80px; height: 80px;
  border: 1px solid var(--border);
  border-radius: 3px;
  object-fit: contain;
  background: #0a0a0a;
  display: block;
  margin: 0 auto 8px;
}
.weapon-desc {
  font-style: italic;
  color: var(--text-dim);
  font-size: 15px;
  line-height: 1.5;
  border-left: 2px solid var(--gold-dk);
  padding-left: 10px;
  margin-top: 8px;
}
.badge-category {
  display: inline-block;
  background: var(--gold-dk);
  color: var(--gold-lt);
  font-family: 'Cinzel', serif;
  font-size: 10px;
  letter-spacing: 1.5px;
  text-transform: uppercase;
  padding: 3px 10px;
  border-radius: 2px;
  margin-bottom: 8px;
}
.scale-grid {
  display: grid;
  grid-template-columns: 1fr 1fr;
  gap: 4px 16px;
}
.info-box {
  background: #0d0b07;
  border-left: 3px solid var(--gold-dk);
  padding: 12px 16px;
  border-radius: 0 3px 3px 0;
  color: var(--text-dim);
  font-size: 17px;
  line-height: 1.6;
  margin-bottom: 12px;
}
.info-box strong { color: var(--gold); font-family: 'Cinzel', serif; font-size: 12px; letter-spacing: 1px; }
.tab-content { padding: 18px 0 0 0 !important; }
.js-plotly-plot .plotly .main-svg { background: transparent !important; }
.divider-rune { text-align: center; color: var(--gold-dk); font-size: 22px; letter-spacing: 14px; margin: 14px 0; }
.about-section-full {
  background: #0d0b07;
  border: 1px solid var(--border);
  border-radius: 3px;
  padding: 20px 24px;
  margin-bottom: 12px;
  width: 100%;
  box-sizing: border-box;
}
.about-section-full h3 {
  font-family: 'Cinzel', serif;
  color: var(--gold);
  font-size: 13px;
  letter-spacing: 2px;
  text-transform: uppercase;
  margin-bottom: 12px;
  padding-bottom: 8px;
  border-bottom: 1px solid var(--border);
}
.about-section-full ul {
  padding-left: 18px;
  color: var(--text-dim);
  font-size: 17px;
  line-height: 1.8;
  margin: 0;
}
.about-section-full li strong { color: var(--gold-lt); }
.about-placeholder {
  color: var(--text-dim);
  font-style: italic;
  font-size: 17px;
  line-height: 1.7;
  margin-bottom: 16px;
  padding: 10px 14px;
  border-left: 2px solid var(--gold-dk);
  background: rgba(201,168,76,0.04);
}
.about-img-wrap {
  margin-top: 12px;
  text-align: center;
}
.about-img {
  max-width: 100%;
  border: 1px solid var(--border);
  border-radius: 3px;
  box-shadow: 0 4px 20px rgba(0,0,0,0.5);
}
.cat-scroll-box {
  max-height: 200px;
  overflow-y: auto;
  border: 1px solid var(--border);
  border-radius: 2px;
  padding: 6px 8px;
  background: #0d0b07;
  scrollbar-width: thin;
  scrollbar-color: var(--gold-dk) var(--bg);
}
.cat-scroll-box::-webkit-scrollbar { width: 5px; }
.cat-scroll-box::-webkit-scrollbar-track { background: var(--bg); }
.cat-scroll-box::-webkit-scrollbar-thumb { background: var(--gold-dk); border-radius: 2px; }
.cat-header { display: flex; justify-content: space-between; align-items: center; margin-bottom: 6px; }
.cat-header-label { color: var(--text-dim); font-family: 'Cinzel', serif; font-size: 13px; letter-spacing: 0.5px; margin: 0; }
.cat-btn {
  font-family: 'Cinzel', serif;
  font-size: 9px;
  letter-spacing: 1px;
  text-transform: uppercase;
  color: #C9A84C;
  background: #0A0A0A;
  border: 1px solid #7A5A10;
  border-radius: 2px;
  padding: 2px 7px;
  cursor: pointer;
  margin-left: 4px;
}
.cat-btn:hover { border-color: #C9A84C; color: #F0D080; }

/* Shield detail */
.shield-detail-wrap { display: flex; flex-direction: column; align-items: center; padding: 20px 0; }
.shield-img-large { width: 120px; height: 120px; object-fit: contain; background: #0a0a0a; border: 1px solid var(--border); border-radius: 3px; display: block; margin-bottom: 14px; }
.shield-desc-block { font-style: italic; color: var(--text-dim); font-size: 16px; line-height: 1.6; border-left: 2px solid var(--gold-dk); padding-left: 14px; max-width: 480px; text-align: left; }

/* Talisman list */
.talisman-scroll { max-height: 70vh; overflow-y: auto; scrollbar-width: thin; scrollbar-color: var(--gold-dk) var(--bg); padding-right: 4px; }
.talisman-scroll::-webkit-scrollbar { width: 5px; }
.talisman-scroll::-webkit-scrollbar-thumb { background: var(--gold-dk); border-radius: 2px; }
.talisman-item { display: flex; align-items: flex-start; gap: 10px; padding: 8px; border: 1px solid var(--border); border-radius: 3px; margin-bottom: 6px; background: #0d0b07; cursor: pointer; transition: border-color 0.15s; }
.talisman-item:hover { border-color: var(--gold-dk); }
.talisman-item.owned { border-color: var(--gold); background: #1a1408; }
.talisman-check { width: 18px; height: 18px; accent-color: var(--gold); flex-shrink: 0; margin-top: 2px; }
#talisman_slots input[type=radio] { accent-color: #C9A84C; }
.slot-row input[type=radio], .slot-row input[type=checkbox] { accent-color: #C9A84C; }
input[type=checkbox], input[type=radio] { accent-color: #C9A84C; }
.talisman-thumb { width: 40px; height: 40px; object-fit: contain; flex-shrink: 0; background: #000; border-radius: 2px; }
.talisman-info { flex: 1; min-width: 0; }
.talisman-name { font-family: 'Cinzel', serif; font-size: 11px; letter-spacing: 0.5px; color: var(--gold-lt); margin-bottom: 3px; }
.talisman-effect { font-size: 15px; color: var(--text-dim); line-height: 1.4; }
.ownership-badge { font-family: 'Cinzel', serif; font-size: 13px; letter-spacing: 1px; color: var(--gold); background: var(--panel); border: 1px solid var(--gold-dk); border-radius: 3px; padding: 6px 14px; text-align: center; margin-bottom: 14px; }
.slot-row { display: flex; align-items: center; gap: 12px; padding: 10px; border: 1px solid var(--border); border-radius: 3px; background: #0d0b07; margin-bottom: 8px; }
.slot-label { font-family: 'Cinzel', serif; font-size: 11px; color: var(--gold-dk); letter-spacing: 1px; width: 52px; flex-shrink: 0; }
.slot-img { width: 44px; height: 44px; object-fit: contain; background: #000; border: 1px solid var(--border); border-radius: 2px; flex-shrink: 0; }
.slot-desc { font-size: 15px; color: var(--text-dim); font-style: italic; flex: 1; line-height: 1.4; }
.slot-name { font-family: 'Cinzel', serif; font-size: 11px; color: var(--gold-lt); margin-bottom: 3px; }
"

fluidPage(
  tags$head(
    tags$style(HTML(elden_css)),
    tags$title("Elden Ring - Helper Dashboard")
  ),
  
  tags$script(HTML("
    var BASE_STYLE   = 'font-family:Cinzel,serif !important; font-size:12px !important; letter-spacing:1.5px !important; text-transform:uppercase !important; border-radius:2px !important; padding:7px 0 !important; width:110px !important; min-width:110px !important; text-align:center !important; display:inline-block !important; box-sizing:border-box !important;';
    var ACTIVE_STYLE  = BASE_STYLE + 'color:#0A0A0A !important; background-color:#C9A84C !important; border:1px solid #C9A84C !important; font-weight:700 !important; box-shadow:0 0 10px rgba(201,168,76,0.4) !important;';
    var INACTIVE_STYLE= BASE_STYLE + 'color:#C9A84C !important; background-color:#0A0A0A !important; border:1px solid #7A5A10 !important; font-weight:400 !important; box-shadow:none !important;';
    var HOVER_STYLE   = BASE_STYLE + 'color:#F0D080 !important; background-color:#1a1408 !important; border:1px solid #C9A84C !important; font-weight:400 !important; box-shadow:none !important;';

    function erStyleTabs() {
      var links = document.querySelectorAll('.nav-pills a, .nav-pills .nav-link');
      links.forEach(function(el) {
        var isActive = el.classList.contains('active') ||
                       el.parentElement.classList.contains('active');
        el.style.cssText = isActive ? ACTIVE_STYLE : INACTIVE_STYLE;

        
        var fresh = el.cloneNode(true);
        el.parentNode.replaceChild(fresh, el);
        fresh.addEventListener('mouseenter', function() {
          if (!fresh.classList.contains('active') && !fresh.parentElement.classList.contains('active')) {
            fresh.style.cssText = HOVER_STYLE;
          }
        });
        fresh.addEventListener('mouseleave', function() {
          var stillActive = fresh.classList.contains('active') || fresh.parentElement.classList.contains('active');
          fresh.style.cssText = stillActive ? ACTIVE_STYLE : INACTIVE_STYLE;
        });
      });
    }

    function waitForTabs() {
      var found = document.querySelectorAll('.nav-pills a, .nav-pills .nav-link');
      if (found.length > 0) {
        erStyleTabs();
        var obs = new MutationObserver(erStyleTabs);
        document.querySelectorAll('.nav-pills').forEach(function(nav) {
          obs.observe(nav, {attributes: true, subtree: true, attributeFilter: ['class']});
        });
      } else {
        setTimeout(waitForTabs, 150);
      }
    }

    document.addEventListener('DOMContentLoaded', function() {
      waitForTabs();
      document.addEventListener('click', function(e) {
        var t = e.target;
        if (t && (t.closest('.nav-pills'))) setTimeout(erStyleTabs, 60);
      });
    });
  ")),
  
  tags$script(HTML("
    (function() {
      var savedScroll = 0;
      $(document).on('shiny:outputinvalidated', function(e) {
        if (e.target && e.target.id === 'talisman_list_ui') {
          var el = document.querySelector('.talisman-scroll');
          if (el) savedScroll = el.scrollTop;
        }
      });
      $(document).on('shiny:value', function(e) {
        if (e.target && e.target.id === 'talisman_list_ui') {
          requestAnimationFrame(function() {
            var el = document.querySelector('.talisman-scroll');
            if (el) el.scrollTop = savedScroll;
          });
        }
      });

      
      function watchTable(tbl) {
        var obs = new MutationObserver(function(mutations) {
          mutations.forEach(function(m) {
            if (m.type === 'attributes' && m.attributeName === 'class') {
              var row = m.target;
              if (row.classList.contains('selected')) {
                Array.prototype.forEach.call(row.querySelectorAll('td'), function(td) {
                  td.style.setProperty('background-color', '#C9A84C', 'important');
                  td.style.setProperty('color', '#0A0A0A', 'important');
                  td.style.setProperty('font-weight', '600', 'important');
                });
              } else {
                Array.prototype.forEach.call(row.querySelectorAll('td'), function(td) {
                  td.style.setProperty('background-color', 'transparent', 'important');
                  td.style.setProperty('color', '#D4C5A0', 'important');
                  td.style.removeProperty('font-weight');
                });
              }
            }
          });
        });
        Array.prototype.forEach.call(tbl.querySelectorAll('tbody tr'), function(row) {
          obs.observe(row, { attributes: true, attributeFilter: ['class'] });
        });
        
        $(tbl).on('draw.dt', function() {
          Array.prototype.forEach.call(tbl.querySelectorAll('tbody tr'), function(row) {
            obs.observe(row, { attributes: true, attributeFilter: ['class'] });
          });
        });
      }
      $(document).on('init.dt', function(e, settings) {
        var tbl = settings.nTable;
        watchTable(tbl);
      });
    })();
  ")),
  
  tags$div(id = "header-banner",
           tags$div(class = "er-monogram", "ER"),
           tags$div(class = "er-title-block",
                    tags$h1("Elden Ring - Helper Dashboard"),
                    tags$p("Between Ancestor and Demigod - Know thy blade, Tarnished")
           )
  ),
  
  tags$div(style = "padding: 16px 20px;",
           tabsetPanel(id = "main_tabs", type = "pills",
                       
                       # TAB 1 - ABOUT
                       tabPanel("Info",
                                tags$br(),
                                tags$div(class = "divider-rune", "* * *"),
                                tags$h2(style = "font-family:'Cinzel',serif; color:#C9A84C; text-align:center; letter-spacing:4px; font-size:22px;",
                                        "ELDEN RING - HELPER DASHBOARD"),
                                tags$p(style = "text-align:center; font-style:italic; color:#8A7B60; margin-bottom:4px;",
                                       "A scholarly compendium of the Lands Between"),
                                tags$div(class = "divider-rune", "* * *"),
                                tags$div(class = "about-section-full",
                                         tags$h3("Optimizer"),
                                         tags$p(class = "about-placeholder",
                                                "The Optimizer lets you input your current character stats - this will also be used in the Shields tab. Use the slider on the left to input your character stats. The table will show only the weapons you are able to wield with your stats. Sort the table by clicking on the appropriate arrows near the column names. After clicking on a specific row, a detail window will appear. You may also use the scrollable category list to choose which weapon types would you like to see. "
                                         ),
                                         tags$div(class = "about-img-wrap",
                                                  tags$img(src = "img_1.png",
                                                           class = "about-img",
                                                           alt = "Optimizer screenshot",
                                                           onerror = "this.style.display='none'")
                                         )
                                ),
                                tags$div(class = "about-section-full",
                                         tags$h3("Stats"),
                                         tags$p(class = "about-placeholder",
                                                "Stats tab lets you explore and compare all the weapons in the game. Simply choose the x-axis and y-axis of your preference. The plot is color-coded by weapon category, so it is easier to compare categories. You can zoom by clicking and dragging and see the details by hovering. "
                                         ),
                                         tags$div(class = "about-img-wrap",
                                                  tags$img(src = "img_2.png",
                                                           class = "about-img",
                                                           alt = "Stats screenshot",
                                                           onerror = "this.style.display='none'")
                                         )
                                ),
                                tags$div(class = "about-section-full",
                                         tags$h3("Damage"),
                                         tags$p(class = "about-placeholder",
                                                "The damage tab allows you to see the damage breakdown of the top N weapons in a category. If you are looking for Katana that also deals fire damage - this is the place to check. Use the weapon category slot to pick which category you are interested in and use the slider to pick how many weapons you would like to compare. You may also pick whether you want a stacked bar or a grouped bar - stacked bar is better if you want to compare the total attack rate, while the grouped bar if you want to see which weapon has deals the most damage of a specific sort."
                                         ),
                                         tags$div(class = "about-img-wrap",
                                                  tags$img(src = "img_3.png",
                                                           class = "about-img",
                                                           alt = "Damage Breakdown screenshot",
                                                           onerror = "this.style.display='none'")
                                         )
                                ),
                                tags$div(class = "about-section-full",
                                         tags$h3("Scaling"),
                                         tags$p(class = "about-placeholder",
                                                "The Scaling tab is designed to help you compare how different weapons with scale with the stat of your choice. Pick the stat you want to use for comparison and use the slider to determine for which levels you want the chart to be. Click at compare weapons to add the weapons you want to compare. Click on the small x near the weapon name to remove it from the comparison. After you change the primary stat, the weapons that do not scale with it will be removed from your selection. If no weapon would persist, it will default to three deafault choices. Hover over the line chart to see details."
                                         ),
                                         tags$div(class = "about-img-wrap",
                                                  tags$img(src = "img_4.png",
                                                           class = "about-img",
                                                           alt = "Scaling Curves screenshot",
                                                           onerror = "this.style.display='none'")
                                         )
                                ),
                                tags$div(class = "about-section-full",
                                         tags$h3("Weight Efficiency"),
                                         tags$p(class = "about-placeholder",
                                                "The weight tab is designated to help you if you want to find the best weapon but within a specific weight treshold. Use the sliders to set the weight limit and the minimum attack rating. Check of which categories you would like to see. Hover for details, click ad drag for zoom."
                                         ),
                                         tags$div(class = "about-img-wrap",
                                                  tags$img(src = "img_5.png",
                                                           class = "about-img",
                                                           alt = "Weight Efficiency screenshot",
                                                           onerror = "this.style.display='none'")
                                         )
                                ),
                                tags$div(class = "about-section-full",
                                         tags$h3("Category Overview"),
                                         tags$p(class = "about-placeholder",
                                                "Categories tab is useful for a general comparison on weapon categories. You can compare by the average total attack rate, the average weight and you can also see the weapon count for each category. It is by default sorted by values by you can change it by checking the box off.  "
                                         ),
                                         tags$div(class = "about-img-wrap",
                                                  tags$img(src = "img_6.png",
                                                           class = "about-img",
                                                           alt = "Category Overview screenshot",
                                                           onerror = "this.style.display='none'")
                                         )
                                ),
                                tags$div(class = "about-section-full",
                                         tags$h3("Shields"),
                                         tags$p(class = "about-placeholder",
                                                "The shields tab lets you compare all the shield in the game. Only the shields you can wield with your current stats are presented. You can sort the table by clicking on the appropriate arrows near column names. After clicking on a specific row, a detail window will apear. At the bottom you can see a plot of all the shield based on their physical defence vs weight - the shields are colored with regard to their category. You can click and drag for zoom, hover for details."
                                         ),
                                         tags$div(class = "about-img-wrap",
                                                  tags$img(src = "img_7.png",
                                                           class = "about-img",
                                                           alt = "Shields screenshot",
                                                           onerror = "this.style.display='none'")
                                         )
                                ),
                                tags$div(class = "about-section-full",
                                         tags$h3("Talismans"),
                                         tags$p(class = "about-placeholder",
                                                "Talismans tab lets you keep track of how many talismans you have collected. Just check off every one of them on the left scroll panel and you will see your colection completeness. Below you can choose how many talisman slots you have unlocked and put the talismans from your colection into the slots. This is useful for planning your your talisman choices, as you can see their full descriptions, which makes is far easier to compare. "
                                         ),
                                         tags$div(class = "about-img-wrap",
                                                  tags$img(src = "img_8.png",
                                                           class = "about-img",
                                                           alt = "Talismans screenshot",
                                                           onerror = "this.style.display='none'")
                                         )
                                ),
                                tags$div(class = "about-section-full",
                                         tags$div(class = "info-box",
                                                  tags$strong("DATA SOURCE"), tags$br(),
                                                  "Weapon statistics sourced from community-aggregated Elden Ring game data from Kaggle ",
                                                  "Scaling grades converted to numeric multipliers: S=1.50, A=1.20, B=0.90, C=0.60, D=0.40, E=0.20. ",
                                                  "AR formula approximates the in-game calculation using a four-tier soft-cap curve."
                                         )
                                )
                       ),
                       
                       # TAB 2 - OPTIMIZER
                       tabPanel("Optimizer",
                                tags$br(),
                                fluidRow(
                                  column(3,
                                         tags$div(class = "sidebar-panel",
                                                  tags$h4("Character Build"),
                                                  sliderInput("str", "Strength",     min=1, max=99, value=18),
                                                  sliderInput("dex", "Dexterity",    min=1, max=99, value=14),
                                                  sliderInput("int", "Intelligence", min=1, max=99, value=10),
                                                  sliderInput("fai", "Faith",         min=1, max=99, value=10),
                                                  sliderInput("arc", "Arcane",        min=1, max=99, value=10),
                                                  tags$hr(),
                                                  tags$div(class = "cat-header",
                                                           tags$span(class = "cat-header-label", "Weapon Categories"),
                                                           tags$span(
                                                             tags$button(class = "cat-btn", id = "btn_all",  "All"),
                                                             tags$button(class = "cat-btn", id = "btn_none", "None")
                                                           )
                                                  ),
                                                  tags$div(class = "cat-scroll-box",
                                                           checkboxGroupInput("cat_filter", label = NULL,
                                                                              choices = character(0), selected = character(0)
                                                           )
                                                  ),
                                                  tags$script(HTML("
                $(document).on('click', '#btn_all', function() {
                  var vals = [];
                  $('#cat_filter input[type=checkbox]').each(function() {
                    $(this).prop('checked', true);
                    vals.push($(this).val());
                  });
                  Shiny.setInputValue('cat_filter', vals, {priority: 'event'});
                });
                $(document).on('click', '#btn_none', function() {
                  $('#cat_filter input[type=checkbox]').prop('checked', false);
                  Shiny.setInputValue('cat_filter', null, {priority: 'event'});
                });
              ")),
                                                  tags$hr(),
                                                  tags$div(class = "info-box",
                                                           "Weapons you cannot wield are hidden.",
                                                           tags$br(),
                                                           "Click a row to inspect the weapon."
                                                  )
                                         )
                                  ),
                                  column(9,
                                         tags$div(class = "er-card",
                                                  tags$div(class = "er-card-title", "Weapon Rankings - Select a Row to Inspect"),
                                                  DTOutput("weapon_table")
                                         ),
                                         tags$div(class = "er-card",
                                                  tags$div(class = "er-card-title", "Weapon Detail"),
                                                  tags$div(id = "weapon-detail-panel",
                                                           uiOutput("weapon_detail")
                                                  )
                                         )
                                  )
                                )
                       ),
                       
                       # TAB 3 - STATS
                       tabPanel("Stats",
                                tags$br(),
                                fluidRow(
                                  column(3,
                                         tags$div(class = "sidebar-panel",
                                                  tags$h4("Explorer Controls"),
                                                  selectInput("explore_x", "X Axis",
                                                              choices = c("Weight"="weight","Base AR"="Base_AR","Total AR"="Total_AR",
                                                                          "Phy ATK"="atk_Phy","Mag ATK"="atk_Mag","Fire ATK"="atk_Fire",
                                                                          "Ligt ATK"="atk_Ligt","Holy ATK"="atk_Holy"),
                                                              selected = "weight"),
                                                  selectInput("explore_y", "Y Axis",
                                                              choices = c("Total AR"="Total_AR","Base AR"="Base_AR","Weight"="weight",
                                                                          "Phy ATK"="atk_Phy","Mag ATK"="atk_Mag","Fire ATK"="atk_Fire",
                                                                          "Ligt ATK"="atk_Ligt","Holy ATK"="atk_Holy"),
                                                              selected = "Total_AR"),
                                                  tags$hr(),
                                                  tags$div(class = "info-box",
                                                           "Each bubble corresponds to a specific weapon",
                                                           tags$br(),
                                                           "Hover for details. Click and drag to zoom."
                                                  )
                                         )
                                  ),
                                  column(9,
                                         tags$div(class = "er-card",
                                                  tags$div(class = "er-card-title", "Weapon Scatter Explorer"),
                                                  plotlyOutput("scatter_plot", height = "500px")
                                         )
                                  )
                                )
                       ),
                       
                       # TAB 4 - DAMAGE
                       tabPanel("Damage",
                                tags$br(),
                                fluidRow(
                                  column(3,
                                         tags$div(class = "sidebar-panel",
                                                  tags$h4("Damage Controls"),
                                                  selectInput("dmg_category", "Weapon Category",
                                                              choices = NULL, multiple = FALSE),
                                                  radioButtons("dmg_chart_type", "Chart Type",
                                                               choices = c("Stacked Bar"="stacked","Grouped Bar"="grouped"),
                                                               selected = "stacked"),
                                                  sliderInput("dmg_top_n", "Show Top N Weapons", min=3, max=20, value=10),
                                                  tags$hr(),
                                                  tags$div(class = "info-box",
                                                           "Compare physical, magical, fire, lightning and holy damage across weapons."
                                                  )
                                         )
                                  ),
                                  column(9,
                                         tags$div(class = "er-card",
                                                  tags$div(class = "er-card-title", "Damage Type Breakdown"),
                                                  plotlyOutput("dmg_bar_plot", height = "500px")
                                         )
                                  )
                                )
                       ),
                       
                       # TAB 5 - SCALING
                       tabPanel("Scaling",
                                tags$br(),
                                fluidRow(
                                  column(3,
                                         tags$div(class = "sidebar-panel",
                                                  tags$h4("Scaling Controls"),
                                                  selectInput("scale_stat", "Primary Stat",
                                                              choices = c("Strength"="Str","Dexterity"="Dex","Intelligence"="Int","Faith"="Fai","Arcane"="Arc"),
                                                              selected = "Str"),
                                                  sliderInput("scale_range", "Stat Range", min=1, max=99, value=c(1,80)),
                                                  selectizeInput("scale_weapons", "Compare Weapons",
                                                                 choices  = NULL,
                                                                 multiple = TRUE,
                                                                 options  = list(
                                                                   plugins = list("remove_button"),
                                                                   maxItems = 5,
                                                                   placeholder = "Select weapons..."
                                                                 )
                                                  ),
                                                  tags$hr(),
                                                  tags$div(class = "info-box",
                                                           "Project how a weapon AR scales as you level up a specific stat."
                                                  )
                                         )
                                  ),
                                  column(9,
                                         tags$div(class = "er-card",
                                                  tags$div(class = "er-card-title", "AR vs Stat Level"),
                                                  plotlyOutput("scaling_line", height = "480px")
                                         )
                                  )
                                )
                       ),
                       
                       # TAB 6 - WEIGHT
                       tabPanel("Weight",
                                tags$br(),
                                fluidRow(
                                  column(3,
                                         tags$div(class = "sidebar-panel",
                                                  tags$h4("Weight Controls"),
                                                  sliderInput("weight_ar_min", "Min Total AR", min=0, max=350, value=50, step=5),
                                                  sliderInput("weight_max",    "Max Weight",   min=0, max=27,  value=15, step=0.5),
                                                  tags$div(class = "cat-header",
                                                           tags$span(class = "cat-header-label", "Category Filter"),
                                                           tags$span(
                                                             tags$button(class = "cat-btn", id = "wt_btn_all",  "All"),
                                                             tags$button(class = "cat-btn", id = "wt_btn_none", "None")
                                                           )
                                                  ),
                                                  tags$div(class = "cat-scroll-box",
                                                           checkboxGroupInput("weight_cat", label = NULL,
                                                                              choices = character(0), selected = character(0)
                                                           )
                                                  ),
                                                  tags$script(HTML("
                $(document).on('click', '#wt_btn_all', function() {
                  var vals = [];
                  $('#weight_cat input[type=checkbox]').each(function() {
                    $(this).prop('checked', true);
                    vals.push($(this).val());
                  });
                  Shiny.setInputValue('weight_cat', vals, {priority: 'event'});
                });
                $(document).on('click', '#wt_btn_none', function() {
                  $('#weight_cat input[type=checkbox]').prop('checked', false);
                  Shiny.setInputValue('weight_cat', null, {priority: 'event'});
                });
              ")),
                                                  tags$hr(),
                                                  tags$div(class = "info-box",
                                                           "Find lightweight weapons that still deliver strong AR."
                                                  )
                                         )
                                  ),
                                  column(9,
                                         tags$div(class = "er-card",
                                                  tags$div(class = "er-card-title", "AR Efficiency vs Weight"),
                                                  plotlyOutput("efficiency_plot", height = "480px")
                                         )
                                  )
                                )
                       ),
                       
                       # TAB 7 - CATEGORIES
                       tabPanel("Categories",
                                tags$br(),
                                fluidRow(
                                  column(3,
                                         tags$div(class = "sidebar-panel",
                                                  tags$h4("Category Controls"),
                                                  radioButtons("cat_metric", "Metric",
                                                               choices = c("Avg Total AR"="Total_AR","Avg Weight"="weight","Count"="count"),
                                                               selected = "Total_AR"),
                                                  checkboxInput("cat_sort", "Sort by Value", value = TRUE),
                                                  tags$hr(),
                                                  tags$div(class = "info-box",
                                                           "Overview of all 31 weapon categories in the game."
                                                  )
                                         )
                                  ),
                                  column(9,
                                         tags$div(class = "er-card",
                                                  tags$div(class = "er-card-title", "Category Comparison"),
                                                  plotlyOutput("category_bar", height = "550px")
                                         )
                                  )
                                )
                       )
                       
                       ,
                       
                       # TAB 8 - SHIELDS
                       tabPanel("Shields",
                                tags$br(),
                                fluidRow(
                                  column(12,
                                         tags$div(class = "er-card",
                                                  tags$div(class = "er-card-title", "Shield Rankings - Select a Row to Inspect"),
                                                  DTOutput("shield_table")
                                         ),
                                         tags$div(class = "er-card",
                                                  tags$div(class = "er-card-title", "Shield Detail"),
                                                  uiOutput("shield_detail")
                                         ),
                                         tags$div(class = "er-card",
                                                  tags$div(class = "er-card-title", "Defence vs Weight"),
                                                  plotlyOutput("shield_scatter", height = "340px")
                                         )
                                  )
                                )
                       ),
                       
                       # TAB 9 - TALISMANS
                       tabPanel("Talismans",
                                tags$br(),
                                fluidRow(
                                  column(4,
                                         tags$div(class = "er-card",
                                                  tags$div(class = "er-card-title", "All Talismans"),
                                                  uiOutput("talisman_list_ui")
                                         )
                                  ),
                                  column(8,
                                         tags$div(style="display:flex; justify-content:center; margin-bottom:10px;",
                                                  uiOutput("talisman_ownership")
                                         ),
                                         tags$div(class = "er-card",
                                                  tags$div(class = "er-card-title", "Talisman Slots"),
                                                  tags$div(style = "margin-bottom: 12px;",
                                                           radioButtons("talisman_slots", "Number of Talisman Slots",
                                                                        choices = c("1"=1,"2"=2,"3"=3,"4"=4),
                                                                        selected = 2, inline = TRUE)
                                                  ),
                                                  uiOutput("talisman_slot_ui")
                                         )
                                  )
                                )
                       )
                       
           ) 
  ) 
) 
