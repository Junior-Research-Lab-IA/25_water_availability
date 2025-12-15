import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

crops = ['vineyards_orchards', 'vegetables']
p0 = {
    'vineyards_orchards': 0.11,
    'vegetables': 0.86
}  # pourcentage initial par crop

a = 0.002                        # increase linear par year
sigma = 0.02                     # écart type du bruit

years = np.arange(2023, 2076)
T = len(years)

# white noise
epsilon = np.random.normal(0, sigma, T)

#matrix initialisation
constante = np.zeros((T, len(crops)))
increase = np.zeros((T, len(crops)))
decrease = np.zeros((T, len(crops)))

#linear computing
for i, crop in enumerate(crops):
    reached_max_inc = False
    reached_min_dec = False
    
    for t in range(T):
        # Constante
        constante[t, i] = np.clip(p0[crop] + epsilon[t], 0, 1)

        # Augmentation
        if not reached_max_inc:
            val_inc = p0[crop] + t*a + epsilon[t]
            if val_inc >= 1:
                reached_max_inc = True
        else:
            val_inc = 1 + epsilon[t]  # bruit sur valeur constante
        increase[t, i] = np.clip(val_inc, 0, 1)

        # Diminution
        if not reached_min_dec:
            val_dec = p0[crop] - t*a + epsilon[t]
            if val_dec <= 0:
                reached_min_dec = True
        else:
            val_dec = 0 + epsilon[t]  # bruit sur valeur constante
        decrease[t, i] = np.clip(val_dec, 0, 1)



# Trends function
def plot_scenario(data, title):
    plt.figure(figsize=(10,6))
    color_map = {
    "vineyards_orchards": "#583000",
    "vegetables": "#ff4c0b",    
    }
    for i, crop in enumerate(crops):
        plt.plot(years, data[:, i], marker='o', label=crop, color=color_map[crop])
    plt.title(title)
    plt.xlabel('Year')
    plt.ylabel('Percentage of irrigated area')
    plt.ylim(0, 1)
    legend_labels = [
    "Vineyards and Orchards", "Vegetables"
    ]
    plt.legend(
        legend_labels,
        title="Crops",
        bbox_to_anchor=(1.05, 1),   # déplace la légende à droite
        loc="upper left",
        borderaxespad=0.
    )
    plt.tight_layout()
    plt.grid(True)
    plt.savefig("prop_irrigsurf_trend_"+title+".png", dpi=300)
    plt.show()

# Plotting trends
plot_scenario(constante, 'Constant irrigation scenario')
plot_scenario(increase, 'Linear increase of irrigation scenario')
plot_scenario(decrease, 'Linear decrease of irrigation scenario')

#exporting csv
scenarios = {'constante': constante, 'increase': increase, 'decrease': decrease}

for name, data in scenarios.items():
    df = pd.DataFrame(data, columns=crops)
    df.insert(0, 'year', years)
    df.to_csv(f'prop_irrigsurf_{name}.csv', index=False)