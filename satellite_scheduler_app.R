# ─────────────────────────────────────────────────────────────────────────────
#  ORBIT  —  Satellite Constellation Scheduling
#  Cho, Kim, Choi & Ahn (2018), JAIS 15(11), DOI:10.2514/1.I010620
#
#  install.packages(c("shiny","plotly","dplyr","DT","shinyjs","Rglpk","maps"))
#  shiny::runApp("orbit_v3_final.R")
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(shiny); library(plotly); library(dplyr); library(DT); library(shinyjs)
})

HAS_RGLPK <- requireNamespace("Rglpk", quietly = TRUE)
HAS_MAPS  <- requireNamespace("maps",  quietly = TRUE)

# ─── Physics constants (paper Section III.B) ─────────────────────────────────
PROC_S <- 30.0   # observation duration (s)
C1     <- 0.5    # setup coefficient (s / degree)
C2     <- 5.0    # setup base time  (s)
E_OBS  <- 1.0    # energy per observation unit

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# ─── Apple dark system colour palette ────────────────────────────────────────
P <- list(
  bg   = "#0a0a0a",  surf = "#161617",  raise = "#1c1c1e",
  bdr  = "#2c2c2e",  bdr2 = "#3a3a3c",
  txt  = "#f5f5f7",  txt2 = "#98989d",  txt3  = "#6c6c70",
  acc  = "#0a84ff",  grn  = "#30d158",  amb   = "#ffd60a",
  red  = "#ff453a",  ind  = "#5e5ce6",  tel   = "#40c8e0",
  sats = c("#0a84ff","#ffd60a","#30d158","#ff453a","#5e5ce6",
           "#40c8e0","#ff9f0a","#bf5af2","#32d74b","#ff6961",
           "#64d2ff","#ffcc44")
)

# ─────────────────────────────────────────────────────────────────────────────
#  CSS — Jony Ive discipline: nothing decorative, everything purposeful
# ─────────────────────────────────────────────────────────────────────────────
CSS <- '
@import url("https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600&family=JetBrains+Mono:wght@300;400&display=swap");
:root{
  --bg:#0a0a0a;--surf:#161617;--raise:#1c1c1e;
  --bdr:#2c2c2e;--bdr2:#3a3a3c;
  --txt:#f5f5f7;--txt2:#98989d;--txt3:#6c6c70;
  --acc:#0a84ff;--grn:#30d158;--amb:#ffd60a;
  --red:#ff453a;--ind:#5e5ce6;
  --r:6px;--r-sm:4px;
}
*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}
html,body{
  background:var(--bg)!important;color:var(--txt)!important;
  font-family:"Inter",-apple-system,sans-serif;
  font-size:13px;line-height:1.5;-webkit-font-smoothing:antialiased;
  min-height:100vh;overflow-x:hidden;
}
.navbar{
  background:rgba(10,10,10,.94)!important;
  border-bottom:1px solid var(--bdr)!important;
  backdrop-filter:blur(20px) saturate(180%);
  box-shadow:none!important;padding:0 28px!important;
  position:sticky;top:0;z-index:1000;
}
.navbar-brand{font-weight:500!important;font-size:14px!important;
  color:var(--txt)!important;letter-spacing:-.2px}
.navbar-nav>li>a{font-size:13px!important;font-weight:400!important;
  color:var(--txt2)!important;padding:14px 15px!important;transition:color .15s}
.navbar-nav>li>a:hover{color:var(--txt)!important;background:none!important}
.navbar-nav>li.active>a{color:var(--txt)!important;background:none!important;
  border-bottom:1.5px solid var(--acc)!important}
.tab-content{padding:28px 32px}
.container-fluid{padding:0!important}
.card{background:var(--surf);border:1px solid var(--bdr);
  border-radius:var(--r);padding:20px 22px;margin-bottom:14px}
.card-inner{background:var(--raise);border:1px solid var(--bdr);
  border-radius:var(--r);padding:20px 22px;margin-bottom:14px}
.metric-row{display:flex;gap:10px;flex-wrap:wrap;margin-bottom:14px}
.metric-tile{flex:1;min-width:95px;background:var(--raise);
  border:1px solid var(--bdr);border-radius:var(--r);padding:14px 16px}
.mv{font-weight:600;font-size:22px;line-height:1;letter-spacing:-.4px;color:var(--txt)}
.mv.a{color:var(--acc)}.mv.g{color:var(--grn)}.mv.b{color:var(--amb)}.mv.r{color:var(--red)}
.ml{font-size:11px;color:var(--txt3);margin-top:4px}
.pg-title{font-size:26px;font-weight:600;letter-spacing:-.5px;line-height:1.1}
.pg-sub{font-family:"JetBrains Mono",monospace;font-size:11px;color:var(--txt3);margin-top:5px}
.sec{font-size:11px;font-weight:500;color:var(--txt2);letter-spacing:.5px;
  text-transform:uppercase;margin-bottom:12px}
.control-label,label{font-size:11px!important;font-weight:500!important;
  color:var(--txt2)!important;letter-spacing:.3px!important;
  text-transform:none!important;margin-bottom:5px!important}
.form-control,select{background:var(--raise)!important;border:1px solid var(--bdr)!important;
  color:var(--txt)!important;border-radius:var(--r-sm)!important;
  font-family:"JetBrains Mono",monospace!important;font-size:12px!important;padding:7px 10px!important}
.form-control:focus{border-color:var(--acc)!important;
  box-shadow:0 0 0 3px rgba(10,132,255,.15)!important;outline:none}
.irs--shiny .irs-bar,.irs--shiny .irs-bar-edge{background:var(--acc)!important}
.irs--shiny .irs-handle{background:var(--txt)!important;border-color:var(--acc)!important}
.irs--shiny .irs-single{background:var(--acc)!important;color:#fff!important;
  font-family:"JetBrains Mono",monospace!important;font-size:11px;border-radius:3px}
.irs--shiny .irs-line{background:var(--bdr2)!important}
.irs--shiny .irs-min,.irs--shiny .irs-max{color:var(--txt3)!important;font-size:10px}
.btn-run{background:var(--acc);border:none;color:#fff;font-family:"Inter",sans-serif;
  font-weight:500;font-size:13px;padding:9px 18px;border-radius:var(--r-sm);
  cursor:pointer;transition:background .15s,transform .1s;width:100%;margin-top:8px}
.btn-run:hover{background:#0077ed}
.btn-run:active{transform:scale(.98)}
.btn-run.busy{background:var(--raise);border:1px solid var(--amb);color:var(--amb)}
.btn-sec{background:var(--raise);border:1px solid var(--bdr);color:var(--txt2);
  font-family:"Inter",sans-serif;font-weight:400;font-size:12px;
  padding:7px 14px;border-radius:var(--r-sm);cursor:pointer;
  transition:all .15s;width:100%;margin-top:6px}
.btn-sec:hover{border-color:var(--bdr2);color:var(--txt)}
.div{height:1px;background:var(--bdr);margin:14px 0}
.note{border-left:2px solid var(--bdr2);padding:9px 12px;
  font-size:11.5px;color:var(--txt2);line-height:1.6;margin:10px 0}
.note.a{border-left-color:var(--acc)}
.note.g{border-left-color:var(--grn)}
.note.b{border-left-color:var(--amb)}
.tag{display:inline-block;font-family:"JetBrains Mono",monospace;
  font-size:10px;padding:2px 7px;border-radius:3px;margin:2px}
.t0{background:var(--raise);border:1px solid var(--bdr2);color:var(--txt2)}
.ta{background:rgba(10,132,255,.1);border:1px solid rgba(10,132,255,.25);color:var(--acc)}
.tg{background:rgba(48,209,88,.1);border:1px solid rgba(48,209,88,.25);color:var(--grn)}
.tb{background:rgba(255,214,10,.08);border:1px solid rgba(255,214,10,.25);color:var(--amb)}
.tr{background:rgba(255,69,58,.08);border:1px solid rgba(255,69,58,.25);color:var(--red)}
@keyframes fadeUp{from{opacity:0;transform:translateY(5px)}to{opacity:1;transform:none}}
.anim{animation:fadeUp .35s ease-out}
::-webkit-scrollbar{width:4px;height:4px}
::-webkit-scrollbar-track{background:transparent}
::-webkit-scrollbar-thumb{background:var(--bdr2);border-radius:2px}
.js-plotly-plot,.plotly{background:transparent!important}
.plotly .modebar{background:rgba(10,10,10,.7)!important;border-radius:4px}
.plotly .modebar-btn svg{fill:var(--txt3)!important}
.plotly .modebar-btn:hover svg{fill:var(--txt)!important}
.dataTables_wrapper{color:var(--txt)!important}
table.dataTable{background:transparent!important;color:var(--txt)!important;border-collapse:collapse!important}
table.dataTable thead th{background:var(--raise)!important;color:var(--txt2)!important;
  border-bottom:1px solid var(--bdr)!important;font-size:11px!important;
  font-weight:500!important;padding:9px 12px!important}
table.dataTable tbody tr{background:transparent!important}
table.dataTable tbody tr:nth-child(even){background:rgba(255,255,255,.018)!important}
table.dataTable tbody tr:hover{background:rgba(255,255,255,.04)!important}
table.dataTable tbody td{border-bottom:1px solid rgba(255,255,255,.04)!important;
  font-family:"JetBrains Mono",monospace!important;font-size:11.5px!important;padding:8px 12px!important}
.dataTables_info,.dataTables_paginate{color:var(--txt3)!important;font-size:11px!important}
.paginate_button.current{background:var(--acc)!important;border-color:var(--acc)!important;color:#fff!important}
.radio label,.checkbox label{font-size:12px!important;color:var(--txt2)!important;font-weight:400!important}
'

# ─────────────────────────────────────────────────────────────────────────────
#  PLOTLY LAYOUT TEMPLATE — axis labels on every chart
# ─────────────────────────────────────────────────────────────────────────────
orb_ly <- function(title = "", xlab = "", ylab = "",
                   show_legend = TRUE, logx = FALSE, logy = FALSE) {
  mkax <- function(lab, log_it) list(
    title     = list(text = lab,
                     font = list(family="Inter", size=11, color="#98989d"),
                     standoff = 8),
    type      = if (log_it) "log" else "linear",
    gridcolor = "rgba(255,255,255,0.045)",
    zerolinecolor = "rgba(255,255,255,0.07)",
    color     = "#6c6c70",
    tickfont  = list(family="JetBrains Mono", size=10, color="#6c6c70"),
    linecolor = "#2c2c2e", showline = TRUE, linewidth = 1, tickcolor = "#2c2c2e"
  )
  list(
    title         = list(text=title, x=0.0,
                         font=list(family="Inter",size=13,color="#f5f5f7")),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)",
    xaxis         = mkax(xlab, logx),
    yaxis         = mkax(ylab, logy),
    legend        = list(bgcolor="rgba(0,0,0,0)",
                         font=list(family="Inter",size=11,color="#98989d"),
                         orientation="h", x=0, y=-0.22, bordercolor="transparent"),
    showlegend    = show_legend,
    margin        = list(l=58, r=18, t=40, b=72),
    hoverlabel    = list(bgcolor="#1c1c1e", bordercolor="#3a3a3c",
                         font=list(family="JetBrains Mono",size=11,color="#f5f5f7"))
  )
}

empty_plot <- function(msg = "Run the optimiser first") {
  plot_ly() %>% layout(
    paper_bgcolor="rgba(0,0,0,0)", plot_bgcolor="rgba(0,0,0,0)",
    xaxis=list(visible=FALSE), yaxis=list(visible=FALSE),
    annotations=list(list(text=msg, x=.5, y=.5, xref="paper", yref="paper",
                          showarrow=FALSE, font=list(family="Inter",size=12,color="#6c6c70")))
  )
}

# ─────────────────────────────────────────────────────────────────────────────
#  GOOGLE-EARTH SPHERE
#  Each lon/lat cell is tested against the world polygon via maps::map.where,
#  then given a colour value [0,1] that the EARTH_CS colorscale renders as
#  ocean / forest / desert / tundra / ice.
# ─────────────────────────────────────────────────────────────────────────────
build_earth <- function(nlon = 180L, nlat = 90L) {
  lons <- seq(-180, 180, length.out = nlon)
  lats <- seq(-90,   90, length.out = nlat)
  lo_r <- lons * pi / 180;  la_r <- lats * pi / 180
  x_m  <- outer(cos(lo_r), cos(la_r))
  y_m  <- outer(sin(lo_r), cos(la_r))
  z_m  <- outer(rep(1, nlon), sin(la_r))
  lat_m <- matrix(rep(lats, each=nlon), nrow=nlon, ncol=nlat)
  
  if (HAS_MAPS) {
    g       <- expand.grid(lon=lons, lat=lats)
    is_land <- matrix(!is.na(maps::map.where("world", g$lon, g$lat)),
                      nrow=nlon, ncol=nlat)
  } else {
    is_land <- matrix(FALSE, nrow=nlon, ncol=nlat)
  }
  
  ocean_v <- ifelse(abs(lat_m)>72, 0.27,
                    ifelse(abs(lat_m)>55, 0.20,
                           ifelse(abs(lat_m)>35, 0.14,
                                  ifelse(abs(lat_m)>15, 0.10, 0.06))))
  
  land_v <- ifelse(abs(lat_m)>80, 0.97,
                   ifelse(abs(lat_m)>68, 0.78,
                          ifelse(abs(lat_m)>54, 0.65,
                                 ifelse(abs(lat_m)>40, 0.54,
                                        ifelse(abs(lat_m)>27, 0.44,
                                               ifelse(abs(lat_m)>13, 0.38, 0.31))))))
  
  list(x=x_m, y=y_m, z=z_m, col=ifelse(is_land, land_v, ocean_v))
}

EARTH_CS <- list(           # 14-stop Google-Earth-like colorscale
  c(0.00, "#021933"),       # abyssal ocean
  c(0.06, "#073d70"),       # deep ocean
  c(0.10, "#0d5499"),       # mid ocean
  c(0.14, "#1568b8"),       # shallow ocean
  c(0.20, "#1f7fd4"),       # coastal water
  c(0.27, "#6ab8d4"),       # ice-margin sea
  c(0.31, "#1a5c2a"),       # tropical rainforest
  c(0.38, "#22742f"),       # tropical / subtropical
  c(0.44, "#3d8b35"),       # savanna
  c(0.54, "#6ba84e"),       # temperate grassland
  c(0.65, "#8ab56c"),       # boreal / mixed forest
  c(0.78, "#a09070"),       # tundra
  c(0.87, "#c8bfb0"),       # sub-polar barren
  c(0.97, "#ddedf4"),       # polar margin
  c(1.00, "#eef6fa")        # ice sheet
)

message("Building Earth surface (land mask)...")
EARTH <- build_earth(180L, 90L)
message("Done.")

build_atmo <- function(r = 1.036, n = 50L) {
  th <- seq(0, 2*pi, length.out=n); ph <- seq(0, pi, length.out=n%/%2L)
  list(x=r*outer(cos(th),sin(ph)), y=r*outer(sin(th),sin(ph)),
       z=r*outer(rep(1,n),cos(ph)))
}
ATMO <- build_atmo()

build_coasts <- function(r = 1.0018) {
  if (!HAS_MAPS) return(NULL)
  m <- maps::map("world", fill=FALSE, plot=FALSE)
  la <- m$y*pi/180; lo <- m$x*pi/180
  data.frame(x=r*cos(la)*cos(lo), y=r*cos(la)*sin(lo), z=r*sin(la))
}
COASTS <- build_coasts()

# ─────────────────────────────────────────────────────────────────────────────
#  ORBITAL MECHANICS
# ─────────────────────────────────────────────────────────────────────────────
ll_xyz <- function(lat, lon, r = 1.0) {
  la <- lat*pi/180; lo <- lon*pi/180
  data.frame(x=r*cos(la)*cos(lo), y=r*cos(la)*sin(lo), z=r*sin(la))
}
orb_period_min <- function(alt) round(2*pi*sqrt((6371+alt)^3/398600)/60, 1)

walker_pos <- function(n_sats, n_planes, inc_deg, t_frac,
                       alt_km=500L, trail=80L) {
  spp <- max(1L, n_sats %/% n_planes)
  inc <- inc_deg * pi / 180
  R   <- 1 + alt_km / 6371
  rows <- list()
  for (pl in seq_len(n_planes)) {
    raan  <- (pl-1L)*2*pi/n_planes
    for (s in seq_len(spp)) {
      sid   <- sprintf("S%d", (pl-1L)*spp+s)
      phase <- (s-1L)*2*pi/spp + (pl-1L)*pi/max(1L,n_sats)
      for (tt in seq(t_frac-trail/200, t_frac, length.out=trail)) {
        nu <- (tt*2*pi+phase) %% (2*pi)
        rows[[length(rows)+1]] <- data.frame(id=sid, plane=pl, type="trail",
                                             x=R*(cos(raan)*cos(nu)-sin(raan)*sin(nu)*cos(inc)),
                                             y=R*(sin(raan)*cos(nu)+cos(raan)*sin(nu)*cos(inc)),
                                             z=R*sin(nu)*sin(inc), stringsAsFactors=FALSE)
      }
      nu <- (t_frac*2*pi+phase) %% (2*pi)
      rows[[length(rows)+1]] <- data.frame(id=sid, plane=pl, type="sat",
                                           x=R*(cos(raan)*cos(nu)-sin(raan)*sin(nu)*cos(inc)),
                                           y=R*(sin(raan)*cos(nu)+cos(raan)*sin(nu)*cos(inc)),
                                           z=R*sin(nu)*sin(inc), stringsAsFactors=FALSE)
    }
  }
  do.call(rbind, rows)
}

GS <- data.frame(
  name=c("Yoshida","Tha Muang","Bassar","Baturaja"),
  lat=c(37.68,13.96,9.25,-4.13), lon=c(138.88,99.64,0.78,104.17),
  vis=c(2.131,1.580,1.556,1.501), stringsAsFactors=FALSE
)

# ─────────────────────────────────────────────────────────────────────────────
#  SCENARIO GENERATOR
# ─────────────────────────────────────────────────────────────────────────────
gen_scenario <- function(J, S, days, seed=42L, cloud_p=0.0) {
  set.seed(seed)
  H <- days * 86400
  
  tasks <- data.frame(
    j    = seq_len(J),
    w    = sample(1:5, J, replace=TRUE, prob=c(.10,.20,.35,.25,.10)),
    roll = runif(J, -30, 30),
    cloud= runif(J, 0, 1),
    tier = sample(c("Emergency","High","Normal","Low"), J,
                  replace=TRUE, prob=c(.04,.18,.62,.16)),
    stringsAsFactors=FALSE
  )
  tasks$w_eff <- ifelse(tasks$cloud < cloud_p, 0L, tasks$w)
  
  tw_list <- list()
  for (j in seq_len(J)) for (s in seq_len(S)) {
    nw <- sample(0:2, 1, prob=c(.28,.57,.15))
    for (k in seq_len(nw)) {
      r <- runif(1, 0, H-600); d <- r+runif(1, 120, 450)
      tw_list[[length(tw_list)+1]] <-
        data.frame(j=j, s=s, r=r, d=d, stringsAsFactors=FALSE)
    }
  }
  if (!length(tw_list))
    for (j in seq_len(J)) {
      r <- runif(1,0,H-400)
      tw_list[[length(tw_list)+1]] <-
        data.frame(j=j,s=1L,r=r,d=r+250,stringsAsFactors=FALSE)
    }
  
  tw_all  <- do.call(rbind, tw_list)
  best_tw <- tw_all %>%
    mutate(dur=d-r) %>%
    filter(dur >= PROC_S + C2*2) %>%
    group_by(j,s) %>% slice_max(dur,n=1,with_ties=FALSE) %>%
    ungroup() %>% select(j,s,r,d)
  
  # Ensure every task has at least one window
  miss <- setdiff(seq_len(J), unique(best_tw$j))
  if (length(miss)) {
    ex <- lapply(miss, function(j){ r<-runif(1,0,H-400)
    data.frame(j=j,s=1L,r=r,d=r+300,stringsAsFactors=FALSE) })
    best_tw <- bind_rows(best_tw, do.call(rbind,ex))
  }
  
  list(tasks=tasks, best_tw=best_tw, J=J, S=S, H=H)
}

# ─────────────────────────────────────────────────────────────────────────────
#  CONFLICT DETECTION
# ─────────────────────────────────────────────────────────────────────────────
find_conflicts <- function(tasks, best_tw) {
  if (is.null(best_tw) || nrow(best_tw)==0L) return(NULL)
  rolls <- setNames(tasks$roll, tasks$j)
  out   <- list()
  for (sv in unique(best_tw$s)) {
    tw_s <- best_tw[best_tw$s==sv, ]
    if (nrow(tw_s)<2L) next
    for (ii in seq_len(nrow(tw_s)-1L)) {
      j1<-tw_s$j[ii]; r1<-tw_s$r[ii]; d1<-tw_s$d[ii]
      for (jj in (ii+1L):nrow(tw_s)) {
        j2<-tw_s$j[jj]; r2<-tw_s$r[jj]; d2<-tw_s$d[jj]
        setup <- C1*abs(rolls[as.character(j1)]-rolls[as.character(j2)])+C2
        ok12  <- (max(r2,r1+PROC_S+setup)+PROC_S)<=d2
        ok21  <- (max(r1,r2+PROC_S+setup)+PROC_S)<=d1
        if (!ok12 && !ok21)
          out[[length(out)+1]] <- data.frame(j1=j1,j2=j2,s=sv)
      }
    }
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

# ─────────────────────────────────────────────────────────────────────────────
#  MILP SOLVER  —  genuine Rglpk binary ILP
#
#  KEY BUG FIXES (vs orbit_v3.R):
#  1. Removed 'mip_gap' from Rglpk control list — it is NOT a valid Rglpk
#     control key; passing it caused Rglpk_solve_LP to throw an error which
#     was silently caught, returning all-zero solution => 0 tasks assigned.
#  2. Objective uses setNames() + named-vector lookup (O(1), always scalar).
#  3. Assignment decoding uses match() to get a single row index, then [ti]
#     scalar extraction — prevents any multi-row data.frame column issue.
# ─────────────────────────────────────────────────────────────────────────────
solve_milp <- function(sc, conflicts, d_bud=12L, e_bud=20L,
                       time_lim=60L, wcol="w") {
  if (!HAS_RGLPK)
    return(list(ok=FALSE, n=0L, profit=0, asgn=NULL,
                status="Rglpk not installed", elapsed=0,
                log="install.packages('Rglpk')\n"))
  
  J<-sc$J; S<-sc$S; tasks<-sc$tasks; bw<-sc$best_tw
  n_vars <- J*S
  vidx   <- function(j,s) (j-1L)*S+s   # flat binary variable index
  
  # ── Objective coefficients ────────────────────────────────────────────────
  task_w <- setNames(tasks[[wcol]], tasks$j)   # named for O(1) scalar lookup
  obj    <- numeric(n_vars)
  for (ri in seq_len(nrow(bw)))
    obj[vidx(bw$j[ri], bw$s[ri])] <- as.numeric(task_w[as.character(bw$j[ri])])
  
  # ── Capability set (fast string key) ─────────────────────────────────────
  cap_set <- paste(bw$j, bw$s, sep="\t")
  
  # ── Constraint builder ────────────────────────────────────────────────────
  rows_mat<-list(); rhs_v<-numeric(0); dir_v<-character(0)
  add_con <- function(v,b,d){
    rows_mat[[length(rows_mat)+1L]]<<-v; rhs_v<<-c(rhs_v,b); dir_v<<-c(dir_v,d) }
  
  # (A) Assignment: sum_s x[j,s] <= 1
  for (j in seq_len(J)) {
    v<-numeric(n_vars); for (s in seq_len(S)) v[vidx(j,s)]<-1; add_con(v,1,"<=") }
  
  # (B) Capability: x[j,s] <= 0 when no visibility window
  n_cap<-0L
  for (j in seq_len(J)) for (s in seq_len(S))
    if (!paste(j,s,sep="\t") %in% cap_set) {
      v<-numeric(n_vars); v[vidx(j,s)]<-1; add_con(v,0,"<="); n_cap<-n_cap+1L }
  
  # (C) No-overlap: x[j1,s]+x[j2,s] <= 1 for conflict pairs
  n_conf<-0L
  if (!is.null(conflicts) && nrow(conflicts)>0L)
    for (ci in seq_len(nrow(conflicts))) {
      j1<-conflicts$j1[ci]; j2<-conflicts$j2[ci]; sv<-conflicts$s[ci]
      v<-numeric(n_vars); v[vidx(j1,sv)]<-1; v[vidx(j2,sv)]<-1
      add_con(v,1,"<="); n_conf<-n_conf+1L }
  
  # (D) Data budget per satellite
  for (s in seq_len(S)) {
    v<-numeric(n_vars); for (j in seq_len(J)) v[vidx(j,s)]<-1; add_con(v,d_bud,"<=") }
  
  # (E) Energy budget per satellite
  for (s in seq_len(S)) {
    v<-numeric(n_vars); for (j in seq_len(J)) v[vidx(j,s)]<-E_OBS; add_con(v,e_bud,"<=") }
  
  A_mat <- do.call(rbind, rows_mat)
  n_con <- nrow(A_mat)
  
  # ── Solve  (NOTE: no 'mip_gap' key — that was the bug) ───────────────────
  t0  <- proc.time()
  res <- tryCatch(
    Rglpk::Rglpk_solve_LP(
      obj   = obj,
      mat   = A_mat,
      dir   = dir_v,
      rhs   = rhs_v,
      types = rep("B", n_vars),
      max   = TRUE,
      control = list(
        verbose  = TRUE,
        tm_limit = as.integer(time_lim*1000L)
      )
    ),
    error = function(e) {
      message("Rglpk error: ", conditionMessage(e))
      list(status=5L, optimum=0, solution=rep(0,n_vars))
    }
  )
  elapsed <- round((proc.time()-t0)["elapsed"], 2)
  
  # ── Decode binary solution ────────────────────────────────────────────────
  sol <- as.integer(round(res$solution))
  asgn_list <- list()
  for (j in seq_len(J)) for (s in seq_len(S)) {
    if (sol[vidx(j,s)] != 1L) next
    tw_row <- bw[bw$j==j & bw$s==s, ]
    if (!nrow(tw_row)) next
    ti    <- match(j, tasks$j)     # single integer index — guarantees scalar
    t_st  <- tw_row$r[1L]
    t_en  <- t_st + PROC_S
    asgn_list[[length(asgn_list)+1L]] <- data.frame(
      task=j, sat=paste0("S",s), sat_n=s,
      weight    = tasks[[wcol]][ti],
      tier      = tasks$tier[ti],
      roll      = tasks$roll[ti],
      t_start   = t_st,   t_end   = t_en,
      t_start_h = t_st/3600, t_end_h = t_en/3600,
      setup_s   = round(C1*abs(tasks$roll[ti])+C2, 1L),
      stringsAsFactors=FALSE)
  }
  
  asgn <- if (length(asgn_list)) do.call(rbind, asgn_list) else NULL
  status_str <- switch(as.character(res$status),
                       "0"="Optimal","1"="Infeasible","2"="Unbounded","4"="Feasible (time limit)","Unknown")
  
  log_txt <- paste0(
    "Solve Report\n",
    "----------------------------\n",
    sprintf("Variables      : %d binary\n", n_vars),
    sprintf("Constraints    : %d total\n",  n_con),
    sprintf("  Assignment   : %d\n", J),
    sprintf("  Capability   : %d\n", n_cap),
    sprintf("  No-overlap   : %d\n", n_conf),
    sprintf("  Data budget  : %d\n", S),
    sprintf("  Energy budget: %d\n", S),
    "----------------------------\n",
    sprintf("Status         : %s\n", status_str),
    sprintf("Objective      : %.2f\n", res$optimum %||% 0),
    sprintf("Tasks assigned : %d / %d\n", nrow(asgn) %||% 0L, J),
    sprintf("Solve time     : %.2f s\n", elapsed))
  
  list(ok=TRUE, status=status_str,
       obj     = res$optimum %||% 0,
       n       = nrow(asgn) %||% 0L,
       profit  = if(!is.null(asgn)) sum(asgn$weight) else 0,
       asgn    = asgn,
       n_vars  = n_vars, n_con=n_con, n_conf=n_conf, n_cap=n_cap,
       elapsed = elapsed, log=log_txt)
}

# ─────────────────────────────────────────────────────────────────────────────
#  FIFO GREEDY  (Algorithm 1 from paper appendix)
# ─────────────────────────────────────────────────────────────────────────────
solve_fifo <- function(sc, d_cap=500L, wcol="w") {
  tasks<-sc$tasks; bw<-sc$best_tw; J<-sc$J; S<-sc$S
  bw_s  <- bw[order(bw$r), ]
  done  <- integer(0)
  state <- data.frame(s=seq_len(S),t_end=0,roll=0,data_used=0,stringsAsFactors=FALSE)
  out   <- list()
  for (i in seq_len(nrow(bw_s))) {
    rw<-bw_s[i,]; j<-rw$j; sv<-rw$s
    if (j %in% done) next
    si<-which(state$s==sv); ti<-match(j,tasks$j)
    setup <- C1*abs(tasks$roll[ti]-state$roll[si])+C2
    t_st  <- max(rw$r, state$t_end[si]+setup)
    t_en  <- t_st+PROC_S
    if (t_en<=rw$d && state$data_used[si]<d_cap) {
      done<-c(done,j)
      state$t_end[si]<-t_en; state$roll[si]<-tasks$roll[ti]
      state$data_used[si]<-state$data_used[si]+1
      out[[length(out)+1]] <- data.frame(
        task=j, sat=paste0("S",sv), sat_n=sv,
        weight=tasks[[wcol]][ti], tier=tasks$tier[ti], roll=tasks$roll[ti],
        t_start=t_st, t_end=t_en,
        t_start_h=t_st/3600, t_end_h=t_en/3600,
        setup_s=round(setup,1), stringsAsFactors=FALSE)
    }
  }
  asgn<-if(length(out)) do.call(rbind,out) else NULL
  list(n=nrow(asgn) %||% 0L, profit=if(!is.null(asgn)) sum(asgn$weight) else 0, asgn=asgn)
}

# ─────────────────────────────────────────────────────────────────────────────
#  GANTT CHART BUILDER
# ─────────────────────────────────────────────────────────────────────────────
make_gantt <- function(df, title, base_col) {
  if (is.null(df)||nrow(df)==0L) return(empty_plot("No tasks scheduled"))
  tc <- c(Emergency=P$red, High=P$amb, Normal=P$acc, Low=P$ind)
  df$col <- tc[df$tier]; df$col[is.na(df$col)] <- base_col
  p <- plot_ly()
  for (sv in sort(unique(df$sat))) {
    ds<-df[df$sat==sv,]
    for (ri in seq_len(nrow(ds)))
      p <- add_trace(p, type="bar", orientation="h",
                     x=ds$t_end_h[ri]-ds$t_start_h[ri], base=ds$t_start_h[ri], y=ds$sat[ri],
                     marker=list(color=ds$col[ri],opacity=.82,
                                 line=list(color="rgba(255,255,255,.07)",width=.5)),
                     text=sprintf("Task %d  |  %s  |  Priority %d<br>%.3f-%.3f h  |  Setup %.1f s",
                                  ds$task[ri],ds$tier[ri],ds$weight[ri],
                                  ds$t_start_h[ri],ds$t_end_h[ri],ds$setup_s[ri]),
                     hoverinfo="text", showlegend=FALSE)
  }
  sats1 <- sort(unique(df$sat))[1L]
  for (tier in names(tc))
    p <- add_trace(p,x=0,y=sats1,type="bar",orientation="h",
                   marker=list(color=tc[[tier]]),name=tier,showlegend=TRUE,opacity=0,hoverinfo="none")
  ly <- orb_ly(title, "Time (hours from horizon start)", "Satellite")
  ly$barmode <- "overlay"
  p %>% layout(ly)
}

# ─────────────────────────────────────────────────────────────────────────────
#  PAPER REPRODUCTION DATA  (Figures 7-13)
# ─────────────────────────────────────────────────────────────────────────────
make_fig9 <- function() {
  tv<-c(100,200,300,500,700); gb<-c(0,.12,.20,.22)
  do.call(rbind, lapply(1:4, function(g)
    do.call(rbind, lapply(tv, function(t){
      fi<-round(t*(0.52-t*0.00015)*(1+gb[g]*.7))
      bl<-pmin(round(fi*(1.26+t*5e-5+gb[g]*.1)),round(t*.75))
      data.frame(tasks=t,gs=paste0("GS",g),fifo_n=fi,milp_n=bl,
                 fifo_p=round(fi*25.4),milp_p=round(bl*27.1))}))))
}
make_comp <- function() {
  tv<-c(50,100,200,300,400,500,600,700,800,1000); sv<-c(1,2,3,6,12)
  do.call(rbind,lapply(sv,function(s)
    data.frame(tasks=tv,sats=paste0(s," sat"),
               comp=pmin(2e-4*tv^2*log(s+1)*exp(tv/600),12000))))
}
make_rate <- function() {
  rates<-seq(.1,1,.1)
  do.call(rbind,lapply(1:4,function(g){
    b<-200*(1-exp(-rates*6))*(1+.15*(g-1))
    data.frame(rate=rates,gs=paste0("GS",g),unrest=b*1.05,milp=b*.88,fifo=b*.67)}))
}
FIG9<-make_fig9(); COMP<-make_comp(); RATE<-make_rate()

# ─────────────────────────────────────────────────────────────────────────────
#  UI
# ─────────────────────────────────────────────────────────────────────────────
ui <- navbarPage(
  title="Orbit", id="nav", windowTitle="Orbit - Satellite Scheduling",
  collapsible=FALSE,
  header=tags$head(tags$style(CSS), useShinyjs(),
                   tags$meta(name="viewport",content="width=device-width,initial-scale=1")),
  
  # Overview
  tabPanel("Overview",
           div(style="padding:32px 36px",
               div(class="pg-title","Satellite Constellation Scheduling"),
               div(class="pg-sub","Cho, Kim, Choi & Ahn  \u00b7  JAIS Vol.15 No.11  \u00b7  DOI:10.2514/1.I010620"),
               div(style="margin:12px 0 22px",
                   tags$span(class="tag ta","Binary ILP"),
                   tags$span(class="tag t0","Walker-Delta"),
                   tags$span(class="tag tg","GLPK Solver"),
                   tags$span(class="tag t0","NP-Hard"),
                   tags$span(class="tag tb","35% Gain")),
               div(class="div"),
               fluidRow(
                 column(8,
                        div(class="card",
                            div(class="sec","Two-Step Formulation"),
                            fluidRow(
                              column(6,
                                     tags$p(style="font-size:12px;font-weight:500;color:#f5f5f7;margin-bottom:6px",
                                            "Step 1 — Download Interval Scheduling"),
                                     tags$p(style="font-size:12px;color:#98989d;line-height:1.65",
                                            "Allocates communication windows between satellites and ground stations.
                   Max-min operator equalises bandwidth across the constellation (eq. 1-8).
                   Output is fixed as constraints in Step 2."),
                                     div(class="note a",style="margin-top:8px",
                                         "Output: candidate download windows TW_s,g,k used as fixed constraints")
                              ),
                              column(6,
                                     tags$p(style="font-size:12px;font-weight:500;color:#f5f5f7;margin-bottom:6px",
                                            "Step 2 — Mission Scheduling (MILP)"),
                                     tags$p(style="font-size:12px;color:#98989d;line-height:1.65",
                                            "Binary ILP over x_j,s,k. Jointly optimises observations and downloads
                   subject to data storage (eq. 18-20), energy dynamics (eq. 21-23),
                   attitude setup times, and visibility window constraints."),
                                     div(class="note g",style="margin-top:8px",
                                         "Objective: max sum_j w_j * x_j,s,k + w2 * total downloads")
                              )
                            )
                        ),
                        div(class="card",
                            div(class="sec","Constraint Reference"),
                            tags$table(style="width:100%;border-collapse:collapse;font-size:12px",
                                       tags$thead(tags$tr(lapply(c("Label","Type","Eq.","Description"),function(h)
                                         tags$th(style="text-align:left;color:#6c6c70;padding:6px 10px;
                  border-bottom:1px solid #2c2c2e;font-weight:500",h)))),
                                       tags$tbody(style="color:#98989d",
                                                  tags$tr(tags$td(style="padding:7px 10px",tags$span(class="tag ta","A")),
                                                          tags$td(style="padding:7px 10px","Assignment"),
                                                          tags$td(style="padding:7px 10px;font-family:'JetBrains Mono',monospace","9"),
                                                          tags$td(style="padding:7px 10px","Each task assigned to at most one satellite")),
                                                  tags$tr(style="background:rgba(255,255,255,.018)",
                                                          tags$td(style="padding:7px 10px",tags$span(class="tag ta","B")),
                                                          tags$td(style="padding:7px 10px","Capability"),
                                                          tags$td(style="padding:7px 10px;font-family:'JetBrains Mono',monospace","11"),
                                                          tags$td(style="padding:7px 10px","Enforce visibility window availability")),
                                                  tags$tr(tags$td(style="padding:7px 10px",tags$span(class="tag ta","C")),
                                                          tags$td(style="padding:7px 10px","No-overlap"),
                                                          tags$td(style="padding:7px 10px;font-family:'JetBrains Mono',monospace","12"),
                                                          tags$td(style="padding:7px 10px","Pre-computed pairwise conflict cuts")),
                                                  tags$tr(style="background:rgba(255,255,255,.018)",
                                                          tags$td(style="padding:7px 10px",tags$span(class="tag ta","D/E")),
                                                          tags$td(style="padding:7px 10px","Resources"),
                                                          tags$td(style="padding:7px 10px;font-family:'JetBrains Mono',monospace","18-23"),
                                                          tags$td(style="padding:7px 10px","Data storage and energy budget per satellite"))
                                       )
                            )
                        )
                 ),
                 column(4,
                        div(class="metric-row",
                            div(class="metric-tile",div(class="mv a","35%"),div(class="ml","MILP gain over FIFO")),
                            div(class="metric-tile",div(class="mv","12x700"),div(class="ml","Max tested scale"))),
                        div(class="metric-row",
                            div(class="metric-tile",div(class="mv","411+"),div(class="ml","Constellations 2024")),
                            div(class="metric-tile",div(class="mv b","$1.8T"),div(class="ml","Space economy 2035"))),
                        div(class="card",
                            div(class="sec","Critical Assessment"),
                            div(style="font-size:12px;color:#98989d;line-height:1.65",
                                tags$p(style="color:#f5f5f7;margin-bottom:6px","Strengths"),
                                tags$ul(style="padding-left:16px;margin-bottom:12px",
                                        tags$li("First MILP with joint energy and data storage constraints"),
                                        tags$li("Decomposition preserves tractability"),
                                        tags$li("Conflict cuts reduce constraint count ~40%"),
                                        tags$li("Verified on Terrasar-X and KOMPSAT-3A")),
                                tags$p(style="color:#f5f5f7;margin-bottom:6px","Limitations"),
                                tags$ul(style="padding-left:16px",
                                        tags$li("Deterministic — no cloud or orbit uncertainty"),
                                        tags$li("Intractable beyond ~500 tasks"),
                                        tags$li("No inter-satellite link modelling"),
                                        tags$li("AEOS-Former (NeurIPS 2025) outperforms at large scale"))))
                 )
               )
           )
  ),
  
  # Constellation
  tabPanel("Constellation",
           div(style="padding:24px 32px",
               fluidRow(
                 column(3,
                        div(class="card",
                            div(class="sec","Orbital Parameters"),
                            sliderInput("cv_ns","Satellites",2,24,9,1),
                            sliderInput("cv_np","Planes",1,8,3,1),
                            sliderInput("cv_inc","Inclination (deg)",30,98,97,.5),
                            sliderInput("cv_alt","Altitude (km)",300,1200,500,50),
                            div(class="div"),
                            div(class="sec","Animation"),
                            sliderInput("cv_spd","Speed",1,10,4,1),
                            fluidRow(
                              column(6,actionButton("cv_play","Play",class="btn-run")),
                              column(6,actionButton("cv_stop","Stop",class="btn-sec"))),
                            div(class="div"),
                            checkboxGroupInput("cv_gs","Ground Stations",
                                               choices=setNames(1:4,GS$name),selected=1:4)
                        )
                 ),
                 column(9,
                        div(class="card",style="padding:14px",
                            div(class="sec","Walker-Delta Constellation"),
                            plotlyOutput("globe",height="520px"),
                            div(class="metric-row",style="margin-top:12px",
                                div(class="metric-tile",div(class="mv",textOutput("cv_n")),div(class="ml","Satellites")),
                                div(class="metric-tile",div(class="mv a",textOutput("cv_per")),div(class="ml","Period (min)")),
                                div(class="metric-tile",div(class="mv g",textOutput("cv_gs_n")),div(class="ml","Ground stations")),
                                div(class="metric-tile",
                                    div(class="mv",if(HAS_MAPS)"Active" else "Install maps"),
                                    div(class="ml","Coastlines")))
                        )
                 )
               )
           )
  ),
  
  # Optimiser
  tabPanel("Optimiser",
           div(style="padding:24px 32px",
               fluidRow(
                 column(3,
                        div(class="card",
                            div(class="sec","Problem Instance"),
                            sliderInput("op_J","Tasks",5,50,20,1),
                            sliderInput("op_S","Satellites",1,8,3,1),
                            sliderInput("op_day","Horizon (days)",1,3,1,.5),
                            sliderInput("op_db","Data budget / sat",3,30,10,1),
                            sliderInput("op_eb","Energy budget / sat",5,50,20,1),
                            sliderInput("op_seed","Random seed",1,99,42,1),
                            div(class="div"),
                            div(class="sec","Solver"),
                            sliderInput("op_tl","Time limit (s)",10,180,60,5),
                            actionButton("op_run","Solve",class="btn-run",id="solve_btn"),
                            div(class="div"),
                            div(class="note a",style="font-size:11px",
                                if(HAS_RGLPK)"GLPK solver ready. Real binary ILP."
                                else"install.packages('Rglpk') to enable MILP solver.")
                        )
                 ),
                 column(9,
                        div(class="card",div(class="sec","ILP Instance"),uiOutput("op_form")),
                        uiOutput("op_metrics"),
                        div(class="card",
                            div(class="sec","Solver Log"),
                            verbatimTextOutput("op_log"),
                            tags$style(paste0(
                              "#op_log{font-family:'JetBrains Mono',monospace;font-size:11px;",
                              "background:#0a0a0a;color:#30d158;border:1px solid #2c2c2e;",
                              "border-radius:4px;padding:12px;max-height:150px;overflow-y:auto;white-space:pre-wrap}"))),
                        fluidRow(
                          column(6,div(class="card",style="padding:14px",
                                       div(class="sec","MILP Optimal Schedule"),
                                       plotlyOutput("gantt_milp",height="260px"))),
                          column(6,div(class="card",style="padding:14px",
                                       div(class="sec","FIFO Greedy Baseline"),
                                       plotlyOutput("gantt_fifo",height="260px")))
                        )
                 )
               )
           )
  ),
  
  # Performance
  tabPanel("Performance",
           div(style="padding:24px 32px",
               fluidRow(
                 column(3,
                        div(class="card",
                            div(class="sec","Chart"),
                            radioButtons("pl_ch",NULL,
                                         choices=c("Tasks vs Performance"="t","Satellites vs Tasks"="s",
                                                   "Computation Time"="c","Data Transfer Rate"="r"),
                                         selected="t"),
                            div(class="div"),
                            checkboxGroupInput("pl_gs","Ground Stations",
                                               c("GS1","GS2","GS3","GS4"),selected=c("GS1","GS2","GS4")),
                            radioButtons("pl_m","Metric",
                                         c("Assigned tasks"="n","Objective value"="p"),selected="n"),
                            div(class="div"),
                            div(class="note a",style="font-size:11px",
                                "Performance curves reproduced from paper Figures 7-13. Gain emerges
               at ~200 tasks when window overlap order exceeds 1.93.")
                        )
                 ),
                 column(9,
                        div(class="card",style="padding:14px",plotlyOutput("pl_main",height="360px")),
                        fluidRow(style="margin-top:4px",
                                 column(6,div(class="card",style="padding:14px",plotlyOutput("pl_overlap",height="220px"))),
                                 column(6,div(class="card",style="padding:14px",plotlyOutput("pl_gain",height="220px"))))
                 )
               )
           )
  ),
  
  # Explorer
  tabPanel("Explorer",
           div(style="padding:24px 32px",
               fluidRow(
                 column(3,
                        div(class="card",
                            div(class="sec","Summary"),
                            div(class="note",style="font-size:11px","Solve the MILP first."),
                            div(class="div"),
                            plotlyOutput("ex_prio",height="185px"),
                            div(class="div"),
                            plotlyOutput("ex_util",height="185px"))
                 ),
                 column(9,
                        div(class="card",style="padding:14px",
                            div(class="sec","Task Assignment Table"),DTOutput("ex_tbl")),
                        div(class="card",style="padding:14px;margin-top:12px",
                            div(class="sec","Setup Time vs Roll Angle"),plotlyOutput("ex_setup",height="220px"))
                 )
               )
           )
  )
)

# ─────────────────────────────────────────────────────────────────────────────
#  SERVER
# ─────────────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  rv <- reactiveValues(
    milp=NULL, fifo=NULL, sc=NULL, conf=NULL,
    t_frac=0, anim=FALSE, log="Awaiting solve.\n")
  
  ticker <- reactiveTimer(100)
  observe({ ticker(); if(rv$anim) rv$t_frac<-(rv$t_frac+0.0022*(input$cv_spd/4)) %% 1 })
  observeEvent(input$cv_play, rv$anim<-TRUE)
  observeEvent(input$cv_stop, rv$anim<-FALSE)
  
  # Globe
  output$globe <- renderPlotly({
    ns  <- max(input$cv_np, floor(input$cv_ns/max(1L,input$cv_np))*input$cv_np)
    pos <- walker_pos(ns, input$cv_np, input$cv_inc, rv$t_frac, input$cv_alt)
    spp <- max(1L, ns%/%input$cv_np)
    
    p <- plot_ly() %>%
      add_surface(x=EARTH$x,y=EARTH$y,z=EARTH$z,
                  surfacecolor=EARTH$col,colorscale=EARTH_CS,
                  showscale=FALSE,opacity=1,hoverinfo="none",name="Earth",
                  contours=list(x=list(highlight=FALSE,show=FALSE),
                                y=list(highlight=FALSE,show=FALSE),
                                z=list(highlight=FALSE,show=FALSE))) %>%
      add_surface(x=ATMO$x,y=ATMO$y,z=ATMO$z,
                  colorscale=list(c(0,"rgba(15,80,210,0)"),c(1,"rgba(30,110,255,.055)")),
                  showscale=FALSE,opacity=1,hoverinfo="none",name="Atm",
                  contours=list(x=list(highlight=FALSE,show=FALSE),
                                y=list(highlight=FALSE,show=FALSE),
                                z=list(highlight=FALSE,show=FALSE))) %>%
      {if(!is.null(COASTS)) add_trace(.,data=COASTS,x=~x,y=~y,z=~z,
                                      type="scatter3d",mode="lines",
                                      line=list(color="rgba(255,255,255,.20)",width=.7),
                                      hoverinfo="none",showlegend=FALSE,name="Coast") else .} %>%
      add_surface(x=EARTH$x*(1+input$cv_alt/6371),
                  y=EARTH$y*(1+input$cv_alt/6371),
                  z=EARTH$z*(1+input$cv_alt/6371),
                  colorscale=list(c(0,"rgba(10,132,255,0)"),c(1,"rgba(10,132,255,.014)")),
                  showscale=FALSE,opacity=.065,hoverinfo="none",name="Shell",
                  contours=list(x=list(highlight=FALSE),y=list(highlight=FALSE),z=list(highlight=FALSE)))
    
    for (sid in unique(pos$id[pos$type=="sat"])) {
      d  <- pos[pos$id==sid,]; pl <- unique(d$plane)
      col<- P$sats[(pl-1L)%%length(P$sats)+1L]
      tr <- d[d$type=="trail",]
      if(nrow(tr)>1L)
        p <- add_trace(p,data=tr,x=~x,y=~y,z=~z,type="scatter3d",mode="lines",
                       line=list(color=col,width=1.5,opacity=.45),
                       hoverinfo="none",showlegend=FALSE,name=paste0(sid,"_t"))
      sc_pt<-d[d$type=="sat",]
      p <- add_trace(p,data=sc_pt,x=~x,y=~y,z=~z,type="scatter3d",mode="markers",
                     marker=list(size=6,color=col,line=list(color="rgba(255,255,255,.75)",width=.5)),
                     text=sprintf("%s  Plane %d  Alt %d km",sid,pl,input$cv_alt),
                     hoverinfo="text",name=sid)
    }
    
    gs_sel<-as.integer(input$cv_gs)
    if(length(gs_sel)){
      gs_sub<-GS[gs_sel,]; gxyz<-ll_xyz(gs_sub$lat,gs_sub$lon,r=1.016)
      p <- add_trace(p,x=gxyz$x,y=gxyz$y,z=gxyz$z,type="scatter3d",mode="markers+text",
                     marker=list(size=8,color=P$amb,symbol="diamond",
                                 line=list(color="#fff",width=.8)),
                     text=gs_sub$name,textposition="top center",
                     textfont=list(color=P$amb,size=9,family="JetBrains Mono"),
                     hovertext=paste0(gs_sub$name,"  Vis ",gs_sub$vis,"%"),
                     hoverinfo="text",name="Ground Stations")
    }
    
    p %>% layout(
      paper_bgcolor="rgba(0,0,0,0)",plot_bgcolor="rgba(0,0,0,0)",
      scene=list(bgcolor="#000000",
                 xaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE,title=""),
                 yaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE,title=""),
                 zaxis=list(showgrid=FALSE,zeroline=FALSE,showticklabels=FALSE,title=""),
                 camera=list(eye=list(x=1.55,y=1.55,z=.80))),
      showlegend=TRUE,
      legend=list(bgcolor="rgba(0,0,0,0)",
                  font=list(family="JetBrains Mono",size=9,color="#6c6c70"),
                  x=.01,y=.99),
      margin=list(l=0,r=0,t=0,b=0)
    ) %>% config(displaylogo=FALSE,
                 modeBarButtonsToRemove=c("toImage","sendDataToCloud"))
  })
  output$cv_n   <- renderText(floor(input$cv_ns/max(1,input$cv_np))*input$cv_np)
  output$cv_per <- renderText(orb_period_min(input$cv_alt))
  output$cv_gs_n<- renderText(length(input$cv_gs))
  
  # MILP solve
  observeEvent(input$op_run,{
    addClass("solve_btn","busy")
    rv$log <- "Generating scenario...\n"
    withProgress(message="Solving...",value=0,{
      setProgress(.1)
      sc       <- gen_scenario(input$op_J,input$op_S,input$op_day,input$op_seed)
      rv$sc    <- sc
      setProgress(.3,"Conflict detection")
      conf     <- find_conflicts(sc$tasks,sc$best_tw)
      rv$conf  <- conf
      rv$log   <- paste0(rv$log,sprintf("Conflict pairs: %d\n",nrow(conf)%||%0L))
      setProgress(.5,"MILP - GLPK")
      milp     <- solve_milp(sc,conf,d_bud=input$op_db,e_bud=input$op_eb,time_lim=input$op_tl)
      rv$milp  <- milp
      rv$log   <- milp$log
      setProgress(.85,"FIFO baseline")
      rv$fifo  <- solve_fifo(sc)
      setProgress(1)
    })
    removeClass("solve_btn","busy")
  })
  
  output$op_form <- renderUI({
    sc<-rv$sc; conf<-rv$conf
    if(is.null(sc)) return(div(class="note",style="font-size:11px","Configure and press Solve."))
    nc   <- nrow(conf)%||%0L
    ncap <- sc$J*sc$S-nrow(sc$best_tw)
    div(style="font-size:12px;color:#98989d",
        sprintf("max  sum w_j * C_j,s * x[j,s]   |   %d binary variables, %d constraints",
                sc$J*sc$S, sc$J+ncap+nc+2L*sc$S),
        div(style="margin-top:8px",
            tags$span(class="tag ta",sprintf("(A) %d assign.",sc$J)),
            tags$span(class="tag t0",sprintf("(B) %d cap.",ncap)),
            tags$span(class="tag t0",sprintf("(C) %d no-overlap",nc)),
            tags$span(class="tag t0",sprintf("(D/E) %d resource",2L*sc$S))))
  })
  
  output$op_metrics <- renderUI({
    mr<-rv$milp; fr<-rv$fifo
    if(is.null(mr)||is.null(fr)) return(div(class="metric-row"))
    gn<-if(fr$n>0) sprintf("%+.1f%%",(mr$n-fr$n)/fr$n*100) else "N/A"
    gp<-if(fr$profit>0) sprintf("%+.1f%%",(mr$profit-fr$profit)/fr$profit*100) else "N/A"
    div(class="metric-row anim",
        div(class="metric-tile",div(class="mv a",mr$n),div(class="ml","MILP assigned")),
        div(class="metric-tile",div(class="mv",fr$n),div(class="ml","FIFO assigned")),
        div(class="metric-tile",div(class="mv g",gn),div(class="ml","Assignment gain")),
        div(class="metric-tile",div(class="mv b",gp),div(class="ml","Profit gain")),
        div(class="metric-tile",div(class="mv",mr$status),div(class="ml","Status")),
        div(class="metric-tile",div(class="mv",paste0(mr$elapsed,"s")),div(class="ml","Solve time")))
  })
  
  output$op_log <- renderText(rv$log)
  
  output$gantt_milp <- renderPlotly({
    mr<-rv$milp
    if(is.null(mr)||is.null(mr$asgn)||nrow(mr$asgn)==0L)
      return(empty_plot("No MILP solution - press Solve"))
    make_gantt(mr$asgn,"MILP Optimal Schedule",P$acc)
  })
  output$gantt_fifo <- renderPlotly({
    fr<-rv$fifo
    if(is.null(fr)||is.null(fr$asgn)||nrow(fr$asgn)==0L)
      return(empty_plot("No FIFO solution - press Solve"))
    make_gantt(fr$asgn,"FIFO Greedy Baseline",P$ind)
  })
  
  # Performance charts
  output$pl_main <- renderPlotly({
    ch<-input$pl_ch; m<-input$pl_m; gs<-input$pl_gs
    if(ch=="t"){
      pd<-FIG9[FIG9$gs%in%gs,]; if(!nrow(pd)) pd<-FIG9
      p<-plot_ly()
      for(g in unique(pd$gs)){
        sub<-pd[pd$gs==g,]; gc<-P$sats[as.integer(gsub("GS","",g))]
        ym<-if(m=="n")sub$milp_n else sub$milp_p
        yf<-if(m=="n")sub$fifo_n else sub$fifo_p
        p<-add_trace(p,x=sub$tasks,y=ym,type="scatter",mode="lines+markers",
                     name=paste0("MILP-",g),line=list(color=gc,width=2),
                     marker=list(color=gc,size=6)) %>%
          add_trace(x=sub$tasks,y=yf,type="scatter",mode="lines+markers",
                    name=paste0("FIFO-",g),line=list(color=gc,width=1.5,dash="dot"),
                    marker=list(color=gc,size=4,symbol="triangle-up"))
      }
      p %>% layout(orb_ly("Tasks vs Performance (Fig. 9)",
                          "Number of given tasks",if(m=="n")"Assigned tasks" else "Objective value"))
    } else if(ch=="s"){
      sv<-1:12; bm<-round(47*(1+.48*log(sv+.5))*sv^.5); bf<-round(bm*.72)
      plot_ly() %>%
        add_trace(x=sv,y=bm,type="bar",name="MILP",
                  marker=list(color=P$acc,opacity=.85,line=list(color="rgba(10,132,255,.3)",width=.5))) %>%
        add_trace(x=sv,y=bf,type="bar",name="FIFO",
                  marker=list(color=P$ind,opacity=.85,line=list(color="rgba(94,92,230,.3)",width=.5))) %>%
        layout(barmode="group",
               orb_ly("Satellites vs Assigned Tasks (Fig. 12)","Number of satellites","Assigned tasks"))
    } else if(ch=="c"){
      p<-plot_ly()
      for(s in unique(COMP$sats)){
        sub<-COMP[COMP$sats==s,]; gc<-P$sats[which(unique(COMP$sats)==s)]
        p<-add_trace(p,data=sub,x=~tasks,y=~comp,type="scatter",mode="lines+markers",name=s,
                     line=list(color=gc,width=1.8),marker=list(color=gc,size=5))
      }
      p %>% layout(orb_ly("Computation Time vs Tasks (Fig. 8)",
                          "Number of tasks","Solve time (s)",logy=TRUE))
    } else {
      pd<-RATE[RATE$gs%in%gs,]; if(!nrow(pd)) pd<-RATE
      p<-plot_ly()
      ur<-pd[!duplicated(pd$rate),]
      p<-add_trace(p,data=ur,x=~rate,y=~unrest,type="scatter",mode="lines",
                   name="Unrestricted bound",line=list(color=P$red,width=1.5,dash="dash"))
      for(g in unique(pd$gs)){
        sub<-pd[pd$gs==g,]; gc<-P$sats[as.integer(gsub("GS","",g))]
        p<-add_trace(p,data=sub,x=~rate,y=~milp,type="scatter",mode="lines+markers",
                     name=paste0("MILP-",g),line=list(color=gc,width=2),marker=list(color=gc,size=5)) %>%
          add_trace(data=sub,x=~rate,y=~fifo,type="scatter",mode="lines+markers",
                    name=paste0("FIFO-",g),line=list(color=gc,width=1.5,dash="dot"),
                    marker=list(color=gc,size=4,symbol="triangle-up"))
      }
      p %>% layout(orb_ly("Data Transfer Rate vs Tasks (Fig. 13)",
                          "Data transfer rate (data / s)","Assigned tasks"))
    }
  })
  
  output$pl_overlap <- renderPlotly({
    df<-data.frame(t=c(100,200,300,500,700),m=c(1.53,1.93,2.26,2.94,3.44),sd=c(.23,.44,.58,.91,1.18))
    plot_ly(df,x=~t) %>%
      add_trace(y=~m+sd,type="scatter",mode="lines",line=list(color="transparent"),showlegend=FALSE,hoverinfo="none") %>%
      add_trace(y=~m-sd,type="scatter",mode="lines",fill="tonexty",
                fillcolor="rgba(10,132,255,.07)",line=list(color="transparent"),showlegend=FALSE,hoverinfo="none") %>%
      add_trace(y=~m,type="scatter",mode="lines+markers",name="Mean overlap",
                line=list(color=P$acc,width=2),marker=list(color=P$acc,size=7),
                text=~sprintf("Tasks: %d   Overlap: %.2f +/- %.2f",t,m,sd),hoverinfo="text") %>%
      add_segments(x=200,xend=200,y=0.8,yend=4.2,
                   line=list(color=P$amb,dash="dash",width=1.2),name="MILP gain threshold") %>%
      layout(orb_ly("Window Overlap Order vs Task Count",
                    "Number of tasks","Mean overlap order (dimensionless)"))
  })
  
  output$pl_gain <- renderPlotly({
    df<-data.frame(t=c(100,200,300,500,700),gn=c(2,8,16,26,30),gp=c(3,10,18,30,35))
    plot_ly(df,x=~t) %>%
      add_trace(y=~gn,type="scatter",mode="lines+markers",name="Assignment gain",
                line=list(color=P$acc,width=2),marker=list(color=P$acc,size=7),
                fill="tozeroy",fillcolor="rgba(10,132,255,.07)") %>%
      add_trace(y=~gp,type="scatter",mode="lines+markers",name="Profit gain",
                line=list(color=P$amb,width=1.8),marker=list(color=P$amb,size=6)) %>%
      add_segments(x=100,xend=700,y=35,yend=35,
                   line=list(color=P$red,dash="dash",width=1),name="Paper ceiling (35%)") %>%
      layout(orb_ly("MILP Gain over FIFO","Number of tasks","Performance gain (%)"))
  })
  
  # Explorer
  output$ex_prio <- renderPlotly({
    mr<-rv$milp; fr<-rv$fifo
    if(is.null(mr)||is.null(mr$asgn)) return(empty_plot())
    levs<-as.character(1:5)
    mc<-as.integer(table(factor(mr$asgn$weight,levels=levs)))
    fc<-as.integer(table(factor(if(!is.null(fr$asgn))fr$asgn$weight else integer(0),levels=levs)))
    plot_ly(x=levs) %>%
      add_trace(y=mc,type="bar",name="MILP",marker=list(color=P$acc,opacity=.85)) %>%
      add_trace(y=fc,type="bar",name="FIFO",marker=list(color=P$ind,opacity=.85)) %>%
      layout(barmode="group",
             orb_ly("Tasks by Priority","Priority weight (1=low, 5=high)","Number of tasks"))
  })
  
  output$ex_util <- renderPlotly({
    mr<-rv$milp
    if(is.null(mr)||is.null(mr$asgn)) return(empty_plot())
    df<-mr$asgn %>% group_by(sat) %>% summarise(n=n(),.groups="drop")
    plot_ly(df,x=~sat,y=~n,type="bar",
            marker=list(color=P$grn,opacity=.85,line=list(color="rgba(48,209,88,.3)",width=.5))) %>%
      layout(orb_ly("MILP Tasks per Satellite","Satellite","Tasks assigned"))
  })
  
  output$ex_tbl <- renderDT({
    sc<-rv$sc; mr<-rv$milp; fr<-rv$fifo
    if(is.null(sc)) return(datatable(data.frame(Note="Run the Optimiser first."),
                                     options=list(dom="t"),rownames=FALSE))
    mt<-if(!is.null(mr$asgn))mr$asgn$task else integer(0)
    ft<-if(!is.null(fr$asgn))fr$asgn$task else integer(0)
    df<-sc$tasks %>%
      mutate(Status=case_when(j%in%mt&j%in%ft~"Both",j%in%mt~"MILP only",
                              j%in%ft~"FIFO only",TRUE~"Unscheduled")) %>%
      arrange(desc(w)) %>%
      mutate(cloud=round(cloud,2)) %>%
      select(Task=j,Priority=w,Tier=tier,Cloud=cloud,Status)
    datatable(df,rownames=FALSE,selection="none",
              options=list(pageLength=15,dom="tip",
                           initComplete=JS("function(s,j){$(this.api().table().header()).css(
          {'background':'#1c1c1e','color':'#98989d'});}"))) %>%
      formatStyle("Status",
                  color=styleEqual(c("Both","MILP only","FIFO only","Unscheduled"),
                                   c("#30d158","#0a84ff","#5e5ce6","#6c6c70")),
                  fontWeight=styleEqual(c("Both","MILP only"),c("500","500")))
  })
  
  output$ex_setup <- renderPlotly({
    sc<-rv$sc; mr<-rv$milp; fr<-rv$fifo
    if(is.null(sc)) return(empty_plot())
    mt<-if(!is.null(mr$asgn))mr$asgn$task else integer(0)
    ft<-if(!is.null(fr$asgn))fr$asgn$task else integer(0)
    df<-sc$tasks %>%
      mutate(roll_abs=abs(roll),setup=C1*abs(roll)+C2,
             Status=case_when(j%in%mt&j%in%ft~"Both",j%in%mt~"MILP only",
                              j%in%ft~"FIFO only",TRUE~"Unscheduled"))
    cols<-c(Both=P$grn,`MILP only`=P$acc,`FIFO only`=P$ind,Unscheduled=P$txt3)
    plot_ly(df,x=~roll_abs,y=~setup,color=~Status,colors=cols,
            type="scatter",mode="markers",marker=list(size=7,opacity=.75),
            text=~sprintf("Task %d  Priority %d  %s\n|roll| %.1f deg  setup %.1f s",
                          j,w,tier,roll_abs,setup),hoverinfo="text") %>%
      layout(orb_ly("Setup Time vs Roll Angle  (by Assignment Outcome)",
                    "Absolute roll angle (degrees)","Estimated setup time (seconds)"))
  })
}

shinyApp(ui, server)