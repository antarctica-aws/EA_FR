#!/usr/bin/env python
# coding: utf-8

# In[1]:


import h5py
import tabulate
import contextily as ctx
import numpy as np
import pandas as pd
import geopandas as gpd
import matplotlib.pyplot as plt
import rasterio
import random
import math
from datetime import datetime, timedelta
# from IPython.display import HTML, display
from os import path
from shapely.geometry import Point
from matplotlib_scalebar.scalebar import ScaleBar
from matplotlib.lines import Line2D
from matplotlib import cm
from matplotlib.colors import ListedColormap
from scipy import stats
import time
import multiprocessing
from multiprocessing import Pool
from rasterio.plot import show
import functools
from osgeo import ogr
from osgeo import gdal
import os
cwd = os.getcwd()
print(cwd)
from glob import glob
from os import path
import rasterio as rio
from rasterio.mask import mask
import pickle


# In[2]:


def back_trans(x):
    x = np.squeeze(x)
    if trans_agbd == 'sqrt':
        return np.square(x)
    else:
        return np.exp(x)


# In[3]:



def image_predict_loop(year, tile, subtile,fpath, model, svi, dst_f, icc=True):  #for now it's for testing the kibale 
    ### 1. apply the desired model to the corresponding landsat image stack 
    # for Kiable there are total of three tiles for LS 173060, 173059, 172060, but moniroting is in 173060
    # 1.1 read in tile 173060 stack as a ndarray 
#     year = '2020'
    # ls_f='ls_gs_medoid_ltr_173060_'+year+'_NBR-0000000000-0000005120.tif'
#     ls_f='ls_gs_medoid_ltr_'+tile+'_'+year+'_NBR-0000002560-0000005120.tif'
#     ls_f ='ls_gs_medoid_ltr_'+tile+'_'+year+'_NBR-'+subtile+'.tif'
    si =set(['ftv_b1_fit_contrast', 'ftv_b1_fit_diss', 'ftv_b1_fit_savg', 'ftv_b1_fit_idm', 'ftv_b1_fit_asm', 
     'ftv_b1_fit_ent', 'ftv_b1_fit_var', 'ftv_b1_fit_corr', 'ftv_b2_fit_contrast', 'ftv_b2_fit_diss', 
     'ftv_b2_fit_savg', 'ftv_b2_fit_idm', 'ftv_b2_fit_asm', 'ftv_b2_fit_ent', 'ftv_b2_fit_var', 'ftv_b2_fit_corr', 
     'ftv_b3_fit_contrast', 'ftv_b3_fit_diss', 'ftv_b3_fit_savg', 'ftv_b3_fit_idm', 'ftv_b3_fit_asm', 
     'ftv_b3_fit_ent', 'ftv_b3_fit_var', 'ftv_b3_fit_corr', 'ftv_b4_fit_contrast', 'ftv_b4_fit_diss', 
     'ftv_b4_fit_savg', 'ftv_b4_fit_idm', 'ftv_b4_fit_asm', 'ftv_b4_fit_ent', 'ftv_b4_fit_var', 'ftv_b4_fit_corr', 
     'ftv_b5_fit_contrast', 'ftv_b5_fit_diss', 'ftv_b5_fit_savg', 'ftv_b5_fit_idm', 'ftv_b5_fit_asm', 
     'ftv_b5_fit_ent', 'ftv_b5_fit_var', 'ftv_b5_fit_corr', 'ftv_b7_fit_contrast', 'ftv_b7_fit_diss', 
     'ftv_b7_fit_savg', 'ftv_b7_fit_idm', 'ftv_b7_fit_asm', 'ftv_b7_fit_ent', 'ftv_b7_fit_var', 'ftv_b7_fit_corr', 
     'NDFI', 'ftv_nbr_fit', 'ftv_ndfi_fit', 'ftv_ndvi_fit', 'ftv_evi_fit', 
     'ftv_ndmi_fit', 'ftv_ndsi_fit', 'ftv_tcw_fit', 'ftv_tcb_fit', 'ftv_tcg_fit', 'ftv_tca_fit', 'ftv_b1_fit', 
     'ftv_b2_fit', 'ftv_b3_fit', 'ftv_b4_fit', 'ftv_b5_fit', 'ftv_b7_fit'])
    
    if icc==True:
        print('AGBD prediction with ICC on Landsat')
        img_stack = gdal.Open(fpath)
        # img_stack = gdal.Open('/gpfs/data1/duncansongp/amberliang/EA_data/ancillary_vars/EAaspect_cropped.tif')
        img_arr = img_stack.ReadAsArray()
        print(type(img_arr))
        img_arr_reshape= img_arr.reshape(img_arr.shape[0], (img_arr.shape[1]*img_arr.shape[2])).transpose()
        
		#read in the ic_df and convert to ic_list and apply ic to lc_raster 
        if tile =='173060':
            t_ic = 'kibale_ic'
        else:
            t_ic = 't'+ tile+'_ic'

        ic_df= pd.read_csv('landsat_comp/'+t_ic+'/ls_predictor_ic_'+ str(year+1)+"-"+str(year)+'.csv',  index_col=0)

        img_arr_reshape_ic = img_arr_reshape*ic_df.values   #np.array(iclist)[None,:]
#         print('new image stack dimension', prev_ic_img_reshape.shape)
#         curr_image_reshape =  prev_ic_img_reshape
        print(img_arr.shape)
        x_pixels = img_arr.shape[1] # number of pixels in x/rows
        y_pixels = img_arr.shape[2] # number of pixels in y/cols
        nbands= img_arr.shape[0]

        img_arr_reshape_ic2= img_arr_reshape_ic.transpose().reshape(nbands, -1,  y_pixels)
        if img_arr_reshape_ic2.shape == img_arr.shape:
            print(img_arr_reshape_ic2.shape, img_arr.shape)
            img_arr = img_arr_reshape_ic2    #overwrite the original img_arr with the ic-ed arr 
            
        #modify the dst_f to indicate it's ic-flag
        dst_f= 'icc_'+dst_f
    else:
        
        print('AGBD preditcion without ICC')
        img_stack = gdal.Open(fpath)
        # img_stack = gdal.Open('/gpfs/data1/duncansongp/amberliang/EA_data/ancillary_vars/EAaspect_cropped.tif')
        img_arr = img_stack.ReadAsArray()
        print(type(img_arr))
        print(img_arr.shape)
        x_pixels = img_arr.shape[1] # number of pixels in x/rows
        y_pixels = img_arr.shape[2] # number of pixels in y/cols
        nbands= img_arr.shape[0]


    bands = {img_stack.GetRasterBand(i).GetDescription(): i for i in range(1, img_stack.RasterCount + 1)}
    bnames =list(bands.keys())
    print(bnames)
    print(len(bnames))
    
    #single out ftv_ndvi_fit as a raster mask
    ndvi_ind=np.where(np.isin(bnames,'ftv_ndvi_fit'))[0].tolist()
    print(ndvi_ind)
    bndvi = img_stack.GetRasterBand(ndvi_ind[0])
    ndvi = bndvi.ReadAsArray()
    print(type(bndvi), type(ndvi))
    
    # continue with the model application 
    bval = img_arr#np.array(bandList)
    bkey = bnames
    img_dict =  {bkey[i]: bval[i] for i in range(len(bkey))}  #dict(zip(bkey, bval)) #
    # print(img_dict)
    print(len(img_dict))

    #stacking the topo vars to the image stack extent
    ra = rio.open(fpath)
    bounds  = ra.bounds
    print(bounds)

    from shapely.geometry import box
    geom = box(*bounds)
    print(geom.wkt)
    df = gpd.GeoDataFrame({"id":1,"geometry":[geom]})
    df= df.set_crs(4326, allow_override=True)
    src  = rasterio.open('/gpfs/data1/duncansongp/amberliang/EA_data/ancillary_vars/topo_stack_res2.tif')
    def getFeatures(gdf):
        """Function to parse features from GeoDataFrame in such a manner that rasterio wants them"""
        import json
        return [json.loads(gdf.to_json())['features'][0]['geometry']]
    coords = getFeatures(df)
    clipped_array, clipped_transform = mask(dataset=src, shapes=coords, crop=True)
    print(clipped_array.shape)
    #append the clipped array to the img_dict 
    tkey = list(src.descriptions)
    tval = clipped_array
    topo_dict = {tkey[i]: tval[i] for i in range(len(tkey))}
    # print(img_dict)
    print(len(topo_dict))
    #append to img_Dict
    img_dict.update(topo_dict)
    print(len(img_dict))
    
    #subset the land cover strata from GLAD product 
    lc=  rasterio.open('ancillary_vars/ea_water/strata_ea_all.tif')
    clipped_array_lc, clipped_transform_lc = mask(dataset=lc, shapes=coords, crop=True)
    print(clipped_array_lc.shape)
    lc_clip_reshape = clipped_array_lc[0]#.reshape(clipped_array_lc[0].shape[0], (clipped_array_lc[0].shape[1]*clipped_array_lc[0].shape[2])).transpose()
    print(lc_clip_reshape.shape)
    
    
    #generate lat and lon layers to extent of the image stacks
    GT = img_stack.GetGeoTransform()
    lon =[]
    lon_layer=[]
    lat = []
    lat_layer=[]
    for i in range(img_arr.shape[1]):
        row_x =[]
        row_y =[]
        for j in range(img_arr.shape[2]):
    #         print(i, j)
            Xpixel =i
            Yline=j
            Xgeo = GT[0] + Xpixel*GT[1] + Yline*GT[2]
            Ygeo = GT[3] + Xpixel*GT[4] + Yline*GT[5]
    #         print(Xgeo, Ygeo)
            row_x.append(Xgeo)
            row_y.append(Ygeo)
        lon.append(row_x)
        lat.append(row_y)
    lon_layer.append(lon)
    lat_layer.append(lat)

    #append the topo and lat/lon to the band dictionaries 
    # img_arr_topo_lon= np.concatenate((img_arr_topo, np.array(lon_layer)), axis=0)
    lon_arr= np.array(lon_layer)
    lat_arr=np.array(lat_layer)
    coord_arr=np.concatenate([lon_arr, lat_arr], axis=0)
    # print(coord_arr.shape)
    ckey =['lon_lowestmode', 'lat_lowestmode']
    coord_dict = {ckey[i]: coord_arr[i] for i in range(len(ckey))}
    img_dict.update(coord_dict)
    print(len(img_dict))
    
    #select bands correpsonding to the model vars ###
    select_vars_del = np.array(ls_vars)[svi] #features[td_bool]
    print(select_vars_del)
    img_arr_model = [img_dict.get(key) for key in select_vars_del]
    print(type(img_arr_model))
    print(len(img_arr_model))
    
    #select_vars_del - for the ones that fall in vis, convet 0s to nan 
#     si_ind=[i for i, val in enumerate(select_vars_del) if val in si]
#     si_ind0=[i for i, val in enumerate(select_vars_del) if not val in si]
#     print(select_vars_del[si_ind])
#     print(si_ind)
#     print(si_ind0)
#     img_arr_model_si = [img_dict.get(key) for key in select_vars_del[si_ind]]  #list of arrays for si
#     img_arr_model_si= list(map(lambda x: np.where(x  == 0, np.nan, x), img_arr_model_si))#replace 0s with nan 
#     img_arr_model_si0 = [img_dict.get(key) for key in select_vars_del[si_ind0]]  #list of arrays for model vars
#     print(type(img_arr_model_si))
#     print(len(img_arr_model))
#     print(len(img_arr_model_si))

#     #add these list elements back to img_dict by the order of si_ind
#     #combine ic and ic2 into one based on indexed position in vind and vind2
#     varlist = img_arr_model_si0
#     print('varlist type: ',type(varlist))
#     for (ind, i) in zip(si_ind, img_arr_model_si):  #vind22 is index of the 1s and ic2 is the value
#         print(ind, i)
#         varlist.insert(ind, i)
# #         print(varlist)
    try:
        #convert list of arrays to ndarray
        img_arr_model2=np.stack(img_arr_model) #np.stack(varlist)
        print(img_arr_model2.shape)

        #apply best model to image stack  ###!!!![need to change]!!!!
        img_arr_reshape = img_arr_model2.reshape(img_arr_model2.shape[0], (img_arr_model2.shape[1]*img_arr_model2.shape[2])).transpose()
        # print(img_arr_reshape.shape)
        print("AGBD predictions being made")
        agbd_nt=model.predict(img_arr_reshape)
        print("AGBD predictions back-transforming")
        agbd = back_trans(agbd_nt)*C    #apply the correction factor C calculated earlier  

        print("AGBD output writing to "+ dst_f)
    #     print(img_arr.shape)
    #     x_pixels = img_arr.shape[1] # number of pixels in x/rows
    #     y_pixels = img_arr.shape[2] # number of pixels in y/cols

        #mask out agbd output arr where ndvi2 is nan before reshaping
        agbd_rsh= np.reshape(agbd, (-1, y_pixels))
        agbd_rsh[np.where(ndvi ==0)] = np.nan   #masking out landsat mask values
        agbd_rsh[np.where(lc_clip_reshape ==16)] = np.nan  #water mask 
        print(agbd_rsh.shape)

        dst_filename = '/gpfs/data1/duncansongp/amberliang/EA_data/agbd_predictions/dodoma2_ts_out/'+dst_f  ###!!! need to change!!!###
        #agbd_'+year+'_xgboost_sqrt_nbr_ann_33_'+ls_f
        # x_pixels = 2560  
        # y_pixels = 2560 # number of pixels in y/cols
        driver = gdal.GetDriverByName('GTiff')
        dataset = driver.Create(dst_filename,x_pixels, x_pixels, 1,gdal.GDT_Float32) #!!note here the larger dimension is used to pad
        dataset.GetRasterBand(1).WriteArray(agbd_rsh)

        # follow code is adding GeoTranform and Projection
    #     img_stack0 = gdal.Open('/gpfs/data1/duncansongp/amberliang/EA_data/landsat_comp/kibale_test/'+ls_f)
        data0= img_stack
        geotrans=data0.GetGeoTransform()  #get GeoTranform from existed 'data0'
        proj=data0.GetProjection() #you can get from a exsited tif or import 
        dataset.SetGeoTransform(geotrans)
        dataset.SetProjection(proj)
        dataset.FlushCache()
        dataset=None
        print("AGBD output finished")
        return(agbd_rsh)
    except: 
        print('predictions cannot be made')
        pass




# In[10]:


###filling in the argument lists 
import glob
import re
ls_vars = ['GDDUR','GDMAG','GDRCH','GDROC','GDTSS','GDTSE','ftv_b1_fit_contrast','ftv_b1_fit_diss','ftv_b1_fit_savg',
         'ftv_b1_fit_idm','ftv_b1_fit_asm','ftv_b1_fit_ent', 'ftv_b1_fit_var','ftv_b1_fit_corr','ftv_b2_fit_contrast',
         'ftv_b2_fit_diss','ftv_b2_fit_savg','ftv_b2_fit_idm','ftv_b2_fit_asm', 'ftv_b2_fit_ent','ftv_b2_fit_var',
         'ftv_b2_fit_corr','ftv_b3_fit_contrast','ftv_b3_fit_diss','ftv_b3_fit_savg','ftv_b3_fit_idm','ftv_b3_fit_asm',
         'ftv_b3_fit_ent','ftv_b3_fit_var','ftv_b3_fit_corr','ftv_b4_fit_contrast','ftv_b4_fit_diss','ftv_b4_fit_savg',
         'ftv_b4_fit_idm','ftv_b4_fit_asm','ftv_b4_fit_ent','ftv_b4_fit_var' ,'ftv_b4_fit_corr', 'ftv_b5_fit_contrast',
         'ftv_b5_fit_diss','ftv_b5_fit_savg','ftv_b5_fit_idm','ftv_b5_fit_asm', 'ftv_b5_fit_ent','ftv_b5_fit_var', 
         'ftv_b5_fit_corr','ftv_b7_fit_contrast','ftv_b7_fit_diss','ftv_b7_fit_savg','ftv_b7_fit_idm','ftv_b7_fit_asm',
         'ftv_b7_fit_ent','ftv_b7_fit_var','ftv_b7_fit_corr','GV','Shade','NPV', 'Soil','NDFI','ftv_nbr_fit',
         'ftv_ndfi_fit','ftv_ndvi_fit','ftv_evi_fit', 'ftv_ndmi_fit','ftv_ndsi_fit',
         'ftv_b1_fit','ftv_b2_fit','ftv_b3_fit','ftv_b4_fit','ftv_b5_fit','ftv_b7_fit','ftv_tcw_fit', 'ftv_tcb_fit',
         'ftv_tcg_fit', 'ftv_tca_fit',
          'ADDUR','ADMAG','ADVAL','ADROC','ADVA5', 'ADMG5','ADRE5','ADRE',
          'BDDUR','BDMAG','BDVAL','BDROC','CTROC','LMDUR','LMMAG','LMROC',
          'slope', 'aspect','dem','lon_lowestmode', 'lat_lowestmode']

pattern2  ='xgboost_sqrt_nbr_lhs_33_dodoma2+2sig_sen97_beam+sol_nbr' ###!!! need to change!!!###

		  
f=pd.read_csv('agbd_predictions/dodoma1_ts_out/model_obj/xgboost_sqrt_nbr_lhs_33_dodoma2+2sig_sen97_beam+sol_nbr_model_Features.csv', header=None) ###!!! need to change!!!###
features = np.array(f.iloc[:,0]).tolist()
print('features',len(features), features)
fm ='agbd_predictions/dodoma1_ts_out/model_obj/xgboost_sqrt_nbr_lhs_33_dodoma2+2sig_sen97_beam+sol_nbr_model.sav' ###!!! need to change!!!###
Cf= pd.read_csv('agbd_predictions/dodoma1_ts_out/model_obj/xgboost_sqrt_nbr_lhs_33_dodoma2+2sig_sen97_beam+sol_nbr_model_C.csv', header=None) ###!!! need to change!!!###
C = Cf.iloc[:,0][0]
print(C)
# ['GDRCH', 'GDTSS', 'ftv_b1_fit_ent', 'ftv_b1_fit_var', 'ftv_b2_fit_diss',
#  'ftv_b2_fit_savg', 'ftv_b2_fit_corr', 'ftv_b4_fit_diss', 'ftv_b4_fit_savg',
#  'ftv_b5_fit_contrast', 'ftv_b5_fit_diss', 'ftv_b7_fit_contrast',
#  'ftv_b7_fit_idm', 'Shade', 'NPV', 'NDFI', 'ftv_ndsi_fit', 'ftv_tcg_fit',
#  'ADROC', 'BDDUR', 'BDMAG', 'BDVAL', 'LMMAG', 'LMROC', 'slope', 'aspect', 'dem',
#  'lon_lowestmode', 'lat_lowestmode']

#########
args=[]
trans_agbd = 'sqrt'
years = range(2020, 2022, 1) ###!!! need to change!!!###
pheno='lhs'
t='168063'   ###!!! need to change!!!###
mod = pickle.load(open(fm, 'rb')) 
tind= np.where(np.isin(ls_vars,features))[0].tolist()
print(len(tind))
svi = tind
iccs =[False]
##########

for i in years:
    yPred = i 
    print(i)
    fitInd='NBR_3by3_dur8_biome1-'
    if t == '173060':
        if i < 2019:
            if i <= 2015:
                print(i)
                fitInd='NBR_3by3_dur8_biome1_val_v2-'  #+'-'+corner+'.tif'
            elif i>2015: 
                print('no')
                fitInd='NBR_3by3_dur8_biome1_val-'  #+'-'+corner+'.tif'
    else:
        fitInd='NBR_3by3_dur8_biome1_val_v2-'  #+'-'+corner+'.tif'
         
            
    imgPattern ='ls_'+pheno+'_medoid_'+t+'_'+str(yPred) +'_'+fitInd + '*.tif' 
    print(imgPattern)    
    if t =='173060':
        t_folder = 'kibale_2000-2021'
    else:
        t_folder = 't'+t+'_1986-2021'

    pdir= "/gpfs/data1/duncansongp/amberliang/EA_data/landsat_comp/"+ t_folder+"/"+ str(yPred)   #EA_data/gd_data/
    annual_pl= glob.glob(path.join(pdir, imgPattern))  
    print('number of subset in tile: '+str(len(annual_pl)))
    
    annual_pl_sub= annual_pl #[s for s in annual_pl if ("0000000000-0000002560" in s)|("0000000000-0000002560" in s)]
    print(annual_pl_sub)
    for subset in annual_pl_sub:
        corner =re.search(fitInd+'(.*).tif', subset)
        corner = corner.group(1)
        print(corner)
        
        ls_fn= 'ls_'+pheno+'_medoid_'+t+'_'+str(yPred)+'_'+fitInd+corner+'.tif' 
    #     ls_fn= 'ls_'+pheno+'_medoid_ltr_'+t+'_'+y+'_NBR_3by3_dur10-'+corner+'.tif'   ####[need to change]###
#         folder =  '/gpfs/data1/duncansongp/amberliang/EA_data/landsat_comp/kibale_2000-2021/'+y+'/'
        fpath = pdir +"/"+ ls_fn
        dst= 'test_agbd_'+str(yPred)+'_'+pattern2+'_'+ls_fn
        print(dst)
        for icc in iccs: 
            arg = yPred, t, corner, fpath, mod, svi, dst, icc
            args.append(arg)
          
          
# In[11]:

len(svi)
len(features)
len(args)


# In[ ]:


print(len(args))
# print(args[0])
# parallel processing 
def main():
    tic = time.time()
    pool = Pool(processes=1)  # set the processes max number 3  ###!!! need to change!!!###
    result = pool.starmap(image_predict_loop, args)
    pool.terminate()
    pool.join()
    print(result)
    print('end')
    toc = time.time()
    print('Done in {:.4f} seconds'.format(toc-tic))

    
if __name__ == "__main__":
    main()


# In[ ]:




