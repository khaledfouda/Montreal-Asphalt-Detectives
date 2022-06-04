import pandas as pd
from pyproj import Transformer

data = pd.read_csv('../data/created/Road_conditions_2019_clean.csv')
transformer = Transformer.from_crs("epsg:32188", "epsg:4326")
data['Latitude'], data['Longitude'] = transformer.transform(data.Coord_X,data.Coord_Y)
data.to_csv('../data/created/Road_conditions_2019_clean_coordinates_fixed.csv', index=False)