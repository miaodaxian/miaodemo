### begin: main data (FRED) ###
fredr_set_key("780eae23dab705bd9ca6f0fa652b38b5")
observation_start = as.Date("1994-01-01")
frequency = "d"
yield_curve = c("DGS1","DGS2","DGS3","DGS5","DGS7","DGS10","DGS20","DGS30") 
### end: main data (FRED) ###

### begin: alternative data (eSignal) ###
name_file_map = list(
  c("tsy_5yr","~/Desktop/ShinyApps/int_ind/FVX.csv"),
  c("tsy_10yr","~/Desktop/ShinyApps/int_ind/TNX.csv"),
  c("tsy_30yr","~/Desktop/ShinyApps/int_ind/TYX.csv")
) 
### end: alternative data (eSignal) ###

### begin: output dir/files and other related specs (e.g. use model ids in output file names and some common sfx and common out dir) ###
out_dir = "~/Desktop/ShinyApps/int_ind/"; 
out_file_sfx = "output_"; #model
out_file_ext = ".csv"; # csv
### end: output dir/files and other related specs (e.g. use model ids in output file names and some common sfx and common out dir) ###

### begin: models: (tree_size,w1,w2,w3) (thresholds) (instruments) (short common description) (name by name desc) ### 
all_models_specs = list(
  list(
    "id1",
    c(60,1,0,0.1),
    c(0.05,0.02,0.3,0.3,0.25,0.25,0.25),
    c("DGS1","DGS3","DGS10","SPY","IBB","XLU","XLF"),
    c("desc common"),
    c()
  ),
  list(
    "id2",
    c(120,1,0,0.1),
    c(0.05),
    c("DGS20"),
    c("desc common"),
    c()
  ),
  list(
    "id3",
    c(125,0.5,0.5,0.1),
    c(0.5,0.7),
    c("USO","XLE"),
    c("desc common"),
      c("USO desc","XLE desc")
    )
  )
### end: models ### 
      
      