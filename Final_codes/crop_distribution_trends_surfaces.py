import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

zone = "zoneC"

# Période d'étude
years = np.arange(2023, 2076)

# Import du Csv
df = pd.read_csv(zone+"base_prop_crop_and_SAU_for_python.csv")

# Mapping R → Python
mapping = {
    "vineyard": "vineyards",
    "orchard": "orchards",
    "vegetable": "vegetables",
    "semiperennial": "semi-perennials",
    "perennial": "other perennials",
    "annual": "other annuals",
    "pasture":"pastures",
    "nonprod": "non productive"
}

# Remplacement des noms
df["type_python"] = df["type_simplified"].map(mapping)

# Création du dict final pour Python
base = df.set_index("type_python")["proportion"].to_dict()

# SAU (unique)
SAU = df["SAU"].iloc[0]


perennials = ["vineyards", "orchards", "other perennials"]
semi_perennials = ["semi-perennials"]
annuals = ["vegetables","other annuals"]
others = ["non productive","pastures"]

def generate_scenario(base, years, SAU, tendance_perennials,scenario_name,
                      noise_annuals=0.02, noise_semi_perennials=0.005):
    """
    Génère un scenario de répartition des surfaces agricoles avec bruit ciblé.
    
    - tendance_perennes : +x => les pérennes augmentent leur part
    - noise_annuelles : écart-type du bruit normal ajouté aux annuelles
    - noise_prairies : écart-type du bruit faible pour les prairies
    """
    
    data = []
    proportions = base.copy()
    
    for year in years:
        t = (year - years[0]) / (years[-1] - years[0])
        delta = tendance_perennials * t
        # Ajustement des pérennes selon la tendance
        new_prop = proportions.copy()
        for k in perennials:
            new_prop[k] = proportions[k] * (1 + delta)
        for k in annuals:
            new_prop[k] = proportions[k] * (1 - delta)
        for k in semi_perennials:
            new_prop[k] = proportions[k] * (1-delta)
        for k in others :
            new_prop[k] = proportions[k]  # valeur de base, bruit ajouté après
        
        # normalisation initiale 
        total = sum(new_prop.values()) 
        new_prop = {k: v / total for k, v in new_prop.items()}
        
        # Ajout de bruit
        for k in annuals:
            new_prop[k] += np.random.normal(0, noise_annuals * new_prop[k])
            new_prop[k] = max(new_prop[k], 0)
        
        # Faible bruit pour les prairies uniquement
        for k in semi_perennials:
            new_prop[k] += np.random.normal(0, noise_semi_perennials * new_prop[k])
            new_prop[k] = max(new_prop[k], 0)

        if "pastures" in others:
            new_prop["pastures"] += np.random.normal(0, noise_semi_perennials * new_prop["pastures"])
            new_prop["pastures"] = max(new_prop["pastures"], 0)

       # 🔹 Renormalisation finale pour SAU fixe 
       # on conserve pérennes et non productive et semi_perennials telles quelles, 
       # et on ajuste seulement les annuelles + semi perennials. 
        fix_cat = perennials + [others[0]]
        total_perennial_fixes = sum(new_prop[k] for k in fix_cat)
        flex_cats = annuals + semi_perennials + [others[1]]
        total_flexibles = sum(new_prop[k] for k in flex_cats)
        
        # on calcule le facteur pour que la somme totale = SAU 
        scale_factor = (1 - total_perennial_fixes) / total_flexibles 
        
        for k in annuals : 
            new_prop[k] *= scale_factor
        
        for k in semi_perennials : 
            new_prop[k] *= scale_factor
        
        if "pastures" in others : 
            new_prop["pastures"] *= scale_factor
        
        # Conversion en hectares
        total = sum(new_prop.values())
        new_prop = {k: v / total * SAU for k, v in new_prop.items()}
        
        # Assignation de l'year
        new_prop["year"] = year
        data.append(new_prop)
    
    df = pd.DataFrame(data).set_index("year")
    df["scenario"] = scenario_name
    return df




# 1. Répartition constante
sc1 = generate_scenario(base, years, SAU, tendance_perennials=0, scenario_name="Constant")

# 2. Pérennes dominantes à long terme
sc2 = generate_scenario(base, years, SAU, tendance_perennials=+0.5, scenario_name="Perennials dominating")

# 3. Annuelles dominantes à long terme
sc3 = generate_scenario(base, years, SAU, tendance_perennials=-0.5, scenario_name="Annuals dominating")

# Combiner
all_scenarios = pd.concat([sc1, sc2, sc3])


import os

output_folder = "resultats_prospective"
os.makedirs(output_folder, exist_ok=True)

for sc_name, df in all_scenarios.groupby("scenario"):
    fig, ax = plt.subplots(figsize=(10, 6))
    color_map = {
    "vineyards": "#580023",
    "orchards": "#058816",
    "vegetables": "#ff4c0b",    
    "other perennials": "#e8e8e8", 
    "semi-perennials": "#cfcfcf",      
    "other annuals": "#959494",
    "pastures": "#787878",
    "non productive": "#5B5B5B"
    }
    order = ["vineyards", "orchards", "vegetables"]  # du bas vers le haut     , "other perennials", "semi-perennials", "other annuals", "pastures", "non productive"
    df_plot = df.drop(columns="scenario")[order]
    legend_labels = [
    "Vineyards", "Orchards", "Vegetables"
    ]

    bars = df_plot.plot(
        kind="bar",
        stacked=True,
        ax=ax,
        color=[color_map[col] for col in df_plot.columns]
    )
    ax.legend(
        legend_labels,
        title="Crops",
        bbox_to_anchor=(1.05, 1),   # déplace la légende à droite
        loc="upper left",
        borderaxespad=0.
    )
    ax.set_title(f"Agricultural land use - {sc_name} - {zone}")
    ax.set_ylabel("Area (ha)")
    ax.set_xlabel("Year")
    ax.set_ylim(0, 3500) 
    plt.tight_layout()
    
    # 🔹 sauvegarde dans le dossier
    filename = f"{sc_name.replace(' ', '_').lower()}"
    plt.savefig(os.path.join(output_folder, zone+"_"+filename+"_landuse_trend.png"), dpi=300)
    plt.show()


for sc_name, df in all_scenarios.groupby("scenario"):
    fig, ax = plt.subplots(figsize=(10, 6))
    color_map = {
    "vineyards": "#580023",
    "orchards": "#058816",
    "vegetables": "#ff4c0b",    
    "other perennials": "#e8e8e8", 
    "semi-perennials": "#cfcfcf",      
    "other annuals": "#959494",
    "pastures": "#787878",
    "non productive": "#5B5B5B"
    }
    order = ["vineyards", "orchards", "vegetables", "other perennials", "semi-perennials", "other annuals", "pastures", "non productive"]  # du bas vers le haut     
    df_plot = df.drop(columns="scenario")[order]
    legend_labels = [
    "Vineyards", "Orchards", "Vegetables", "Other perennials", "Semi-perennials", "Other annuals", "Pastures", "Non productive area"
    ]

    bars = df_plot.plot(
        kind="bar",
        stacked=True,
        ax=ax,
        color=[color_map[col] for col in df_plot.columns]
    )
    ax.legend(
        legend_labels,
        title="Crops",
        bbox_to_anchor=(1.05, 1),   # déplace la légende à droite
        loc="upper left",
        borderaxespad=0.
    )
    ax.set_title(f"Total agricultural landuse - {sc_name} - {zone}")
    ax.set_ylabel("Area (ha)")
    ax.set_xlabel("Year")
    plt.tight_layout()
    # 🔹 sauvegarde dans le dossier
    filename = f"{sc_name.replace(' ', '_').lower()}"
    plt.savefig(os.path.join(output_folder, zone+"_"+filename+"_total_landuse_trend.png"), dpi=300)
    plt.show()

sc1_subset = df[["vineyards", "orchards", "vegetables"]]
sc2_subset = df[["vineyards", "orchards", "vegetables"]]
sc3_subset = df[["vineyards", "orchards", "vegetables"]]

sc1_subset.to_csv(os.path.join(output_folder, "landuse_"+zone+"_constante.csv"), index=True)
sc2_subset.to_csv(os.path.join(output_folder, "landuse_"+zone+"_perennialsdominating.csv"), index=True)
sc3_subset.to_csv(os.path.join(output_folder, "landuse_"+zone+"_annualsdominating.csv"), index=True)

all_scenarios.to_csv(os.path.join(output_folder, "landuse"+zone+"_allscenarios.csv"), index=True)
