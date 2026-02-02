




#' FA data work
#' 
#'
#' @export
fa_dwork_clean <- function(df) {

  {{ df }} %>%
    group_by(dnum) %>%
    arrange(FY) %>%
    mutate(cy.d.8.pupilcnt.chg = cy.d.8.pupilcnt - lag(cy.d.8.pupilcnt),
           cy.d.8.pupilcnt.pct.chg = (cy.d.8.pupilcnt.chg / lag(cy.d.8.pupilcnt)) * 100,
           cy.d.8.found.chg = cy.d.8.found.pp - lag(cy.d.8.found.pp),
           cy.d.8.found.pct.chg = (cy.d.8.found.chg / lag(cy.d.8.found.pp)) * 100,

           cy.d.8.found.rev = cy.d.8.found.pp * cy.d.8.pupilcnt,
           cy.d.8.found.rev.chg = cy.d.8.found.rev - lag(cy.d.8.found.rev),
           cy.d.8.found.rev.pct.chg = (cy.d.8.found.rev.chg / lag(cy.d.8.found.rev)) * 100,
           cy.d.8.found.rev.local = cy.d.8.revenuepp * cy.d.8.gepupils,


           cy.d.1.found.rev = cy.d.1.found.pp * cy.d.1.pupilcnt,
           cy.d.1.found.rev.local = cy.d.1.revenuepp * cy.d.1.gepupils,
           # need to use GE pupils

           cy.d.pupilcnt = ifelse(is.na(cy.d.8.pupilcnt), cy.d.1.pupilcnt, cy.d.8.pupilcnt),
           cy.d.pupilcnt.chg = cy.d.pupilcnt - lag(cy.d.pupilcnt),
           cy.d.pupilcnt.pct.chg = (cy.d.pupilcnt.chg / lag(cy.d.pupilcnt)) * 100,
           
           cy.d.pylunch = ifelse(cy.d.8.pylunch == 0 | is.na(cy.d.8.pylunch), cy.d.1.pylunch, cy.d.8.pylunch),
           cy.d.pylunch.chg = cy.d.pylunch - lag(cy.d.pylunch),
           cy.d.pylunch.pct.chg = (cy.d.pylunch.chg / lag(cy.d.pylunch)) * 100,
           cy.d.pylunch.pct.of.pupilcnt = (cy.d.pylunch / cy.d.pupilcnt) * 100, 
            
    
           cy.d.found.pp = ifelse(is.na(cy.d.8.found.pp), cy.d.1.found.pp, cy.d.8.found.pp),
           cy.d.found.pp.chg = cy.d.found.pp - lag(cy.d.found.pp),
           cy.d.found.pp.pct.chg = (cy.d.found.pp.chg / lag(cy.d.found.pp)) * 100,

           cy.d.found.rev = ifelse(is.na(cy.d.8.found.rev), cy.d.1.found.rev, cy.d.8.found.rev),
           cy.d.found.rev.chg = cy.d.found.rev - lag(cy.d.found.rev),
           cy.d.found.rev.pct.chg = (cy.d.found.rev.chg / lag(cy.d.found.rev)) * 100

    ) %>%
    ungroup() %>%

    group_by(FY) %>%
    mutate(enroll.state.8 = sum(cy.d.8.pupilcnt, na.rm = TRUE) / 2, 
           # dividing by 2 because ISDs include all the membership of all their constituent districts
           enroll.state.1 = sum(cy.d.1.pupilcnt, na.rm = TRUE) / 2,
           enroll.pct.state.8 = cy.d.8.pupilcnt / enroll.state.8,
           enroll.pct.state.1 = cy.d.1.pupilcnt / enroll.state.1) %>%
    ungroup() %>%

    
    # this is what I'm changing. This should fix it but I'm worried it'll break something else 
    mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
    mutate(fid.r.total.audit = ifelse(fid.r.total.audit == 0, NA, fid.r.total.audit), 
           db.original.total.revenue = ifelse(db.original.total.revenue == 0, NA, db.original.total.revenue)) %>% 
    


    # FID
    group_by(dnum) %>%
    arrange(FY) %>%
    mutate(fid.r.total.audit.chg = fid.r.total.audit - lag(fid.r.total.audit),
           fid.r.total.audit.pct.chg = (fid.r.total.audit.chg / lag(fid.r.total.audit)) * 100,
           
           fid.e.total.xtrans = ifelse(fid.e.total.xtrans == 0, NA, fid.e.total.xtrans),
           fid.e.total.xtrans.chg = fid.e.total.xtrans - lag(fid.e.total.xtrans),
           fid.e.total.xtrans.pct.chg = (fid.e.total.xtrans.chg / lag(fid.e.total.xtrans)) * 100,

           fid.surplus.deficit = fid.r.total.audit - fid.e.total.xtrans,
           fid.rev.expended  = (fid.e.total.xtrans / fid.r.total.audit) * 100,

           fid.b.fb.11.chg = fid.b.fb.11 - lag(fid.b.fb.11),
           fid.b.fb.11.pct.chg = (fid.b.fb.11.chg / lag(fid.b.fb.11)) * 100,
           fid.b.fb.11.pct.rev = (fid.b.fb.11 / fid.r.total.audit) * 100,
           fid.b.fb.11.pct.exp = (fid.b.fb.11 / fid.e.total.xtrans) * 100,

           fid.e.fed.stim.total = fid.e.fed.stim.esser1 + fid.e.fed.stim.esser2 +
             fid.e.fed.stim.esser3 + fid.e.fed.stim.geer1 + fid.e.fed.stim.equity +
             fid.e.fed.stim.essereq2 + fid.e.fed.stim.essereq3
           # + fid.e.fed.stim.crf1 + fid.e.fed.stim.crf2
           # I took this out because some districts got extra money through the counties I couldn't track. In any case it's all spent
           ,
           

           fid.e.fed.stim.sum.esser1 = sum(fid.e.fed.stim.esser1, na.rm = TRUE),
           fid.e.fed.stim.sum.esser2= sum(fid.e.fed.stim.esser2, na.rm = TRUE),
           fid.e.fed.stim.sum.esser3 = sum(fid.e.fed.stim.esser3, na.rm = TRUE),
           fid.e.fed.stim.sum.geer1 = sum(fid.e.fed.stim.geer1, na.rm = TRUE) ,
           fid.e.fed.stim.sum.equity = sum(fid.e.fed.stim.equity, na.rm = TRUE),
           fid.e.fed.stim.sum.crf1 = sum(fid.e.fed.stim.crf1, na.rm = TRUE),
           fid.e.fed.stim.sum.crf2 = sum(fid.e.fed.stim.crf2, na.rm = TRUE),
           fid.e.fed.stim.sum.essereq2 = sum(fid.e.fed.stim.essereq2, na.rm = TRUE),
           fid.e.fed.stim.sum.essereq3 = sum(fid.e.fed.stim.essereq3, na.rm = TRUE),
           fid.e.fed.stim.sum.total = sum(fid.e.fed.stim.total, na.rm = TRUE)
    ) %>%
    ungroup() %>%

    # EEM

    group_by(dnum) %>%
    mutate(stim.alloc.total = stim.alloc.esser1 + stim.alloc.esser2 +
             stim.alloc.esser3 + stim.alloc.geer1 + stim.alloc.equity +
             stim.alloc.essereq2 + stim.alloc.essereq3
           # + stim.alloc.crf1 + stim.alloc.crf2
           # removing this bit along with the expenditure side to correct for some issues
    ) %>%


    # Budget

    mutate(db.original.total.revenue = ifelse(db.original.total.revenue == 0, NA, db.original.total.revenue),
           db.final.total.revenue  = ifelse(db.final.total.revenue == 0, NA, db.final.total.revenue),
           db.original.total.expenditure = ifelse(db.original.total.expenditure == 0, NA, db.original.total.expenditure),
           db.final.total.expenditure = ifelse(db.final.total.expenditure == 0, NA, db.final.total.expenditure),
           
           db.orig.rev.error = db.original.total.revenue - fid.r.total.audit,
           db.final.rev.error = db.final.total.revenue - fid.r.total.audit,
           db.orig.rev.pct.act = (db.original.total.revenue / fid.r.total.audit) * 100,
           db.final.rev.pct.act = (db.final.total.revenue / fid.r.total.audit) * 100,

           db.orig.exp.error = db.original.total.expenditure - fid.e.total.xtrans,
           db.final.exp.error = db.final.total.expenditure - fid.e.total.xtrans,
           db.orig.exp.pct.act = (db.original.total.expenditure / fid.e.total.xtrans) * 100,
           db.final.exp.pct.act = (db.final.total.expenditure / fid.e.total.xtrans) * 100,
           
           db.orig.surplus.deficit = db.original.total.revenue - db.original.total.expenditure,
           db.final.surplus.deficit = db.final.total.revenue - db.final.total.expenditure, 
           db.orig.surplus.error = db.orig.surplus.deficit - fid.surplus.deficit,
           db.final.surplus.error = db.final.surplus.deficit - fid.surplus.deficit
           
    ) %>%
    ungroup() %>%
    # tt_dnum_psa(dnum = dnum) %>%
    # tt_dnum_isd(dnum = dnum) %>%
    # filter(flag.charter == 0,
    #        flag.isd == 0) %>%


    # Transformations for ML Process

    arrange(dnum, FY) %>%
    mutate(
      outcome = fid.r.total.audit - fid.r.fed.stim,
      outcome.x1 = lag(outcome),

      found.rev.pct.chg = (cy.d.found.rev - lag(cy.d.found.rev)) / lag(cy.d.found.rev),

      fid.r.total.x1 = lag(fid.r.total),
      fid.r.total.x2 = lag(fid.r.total.x1),
      fid.r.total.x3 = lag(fid.r.total.x2),
      fid.r.total.x4 = lag(fid.r.total.x3),
      fid.r.total.x5 = lag(fid.r.total.x4),
      fid.r.total.chg.2 = (fid.r.total.x1 - fid.r.total.x2) / fid.r.total.x2,
      fid.r.total.chg.3 = (fid.r.total.x1 - fid.r.total.x3) / fid.r.total.x3,
      fid.r.total.chg.5 = (fid.r.total.x1 - fid.r.total.x5) / fid.r.total.x5,

      cy.d.1.pupilcnt = ifelse(cy.d.1.pupilcnt == 0, cy.d.8.pupilcnt, cy.d.1.pupilcnt),
      # I'm cheating a tiny bit here because I don't have february measurments
      # for pupil count earlier than 2011. But I have August ones.


      cy.d.1.pupilcnt.x1 = lag(cy.d.1.pupilcnt),
      cy.d.1.pupilcnt.x2 = lag(cy.d.1.pupilcnt.x1),
      cy.d.1.pupilcnt.x3 = lag(cy.d.1.pupilcnt.x2),
      cy.d.1.pupilcnt.x4 = lag(cy.d.1.pupilcnt.x3),
      cy.d.1.pupilcnt.x5 = lag(cy.d.1.pupilcnt.x4),
      cy.d.1.pupilcnt.chg.1 = (cy.d.1.pupilcnt - cy.d.1.pupilcnt.x1) / cy.d.1.pupilcnt.x1,
      cy.d.1.pupilcnt.chg.2 = (cy.d.1.pupilcnt - cy.d.1.pupilcnt.x2) / cy.d.1.pupilcnt.x2,
      cy.d.1.pupilcnt.chg.3 = (cy.d.1.pupilcnt - cy.d.1.pupilcnt.x3) / cy.d.1.pupilcnt.x3,
      cy.d.1.pupilcnt.chg.5 = (cy.d.1.pupilcnt - cy.d.1.pupilcnt.x5) / cy.d.1.pupilcnt.x5,


      cy.d.pupilcnt.x1 = lag(cy.d.pupilcnt),
      cy.d.pupilcnt.x2 = lag(cy.d.pupilcnt.x1),
      cy.d.pupilcnt.x3 = lag(cy.d.pupilcnt.x2),
      cy.d.pupilcnt.x4 = lag(cy.d.pupilcnt.x3),
      cy.d.pupilcnt.x5 = lag(cy.d.pupilcnt.x4),
      cy.d.pupilcnt.pct.chg.1 = (cy.d.pupilcnt - cy.d.pupilcnt.x1) / cy.d.pupilcnt.x1,
      cy.d.pupilcnt.pct.chg.2 = (cy.d.pupilcnt - cy.d.pupilcnt.x2) / cy.d.pupilcnt.x2,
      cy.d.pupilcnt.pct.chg.3 = (cy.d.pupilcnt - cy.d.pupilcnt.x3) / cy.d.pupilcnt.x3,
      cy.d.pupilcnt.pct.chg.5 = (cy.d.pupilcnt - cy.d.pupilcnt.x5) / cy.d.pupilcnt.x5,


    ) %>%


    # District Details Variables
    group_by(dnum) %>%
    mutate(enroll.min = as.character(comma(round(min(cy.d.pupilcnt), digits = -1), digits = 0)),
           enroll.max = as.character(comma(round(max(cy.d.pupilcnt), digits = -1), digits = 0)),
           enroll.range = paste(enroll.min, " - ", enroll.max),
           district.notes = NA) %>%
    ungroup()




}

