import numpy as np
import csv
import math

import matplotlib.pyplot as plt
from matplotlib.patches import Circle, RegularPolygon
from matplotlib.path import Path
from matplotlib.projections.polar import PolarAxes
from matplotlib.projections import register_projection
from matplotlib.spines import Spine
from matplotlib.transforms import Affine2D

segments = ['Taxi', 'Plataforma', 'Corporativo', 'MiPyme', 'Particular', 'Transporte de Personal']
data_dict = {
    'Taxi': {
        '20': 0,
        '30': 0,
        '40': 0,
        '50': 0,
        '60': 0,
        '70': 0,
        '80': 0,
    },
    'Plataforma': {
        '20': 0,
        '30': 0,
        '40': 0,
        '50': 0,
        '60': 0,
        '70': 0,
        '80': 0,
    },
    'Corporativo': {
        '20': 0,
        '30': 0,
        '40': 0,
        '50': 0,
        '60': 0,
        '70': 0,
        '80': 0,
    },
    'MiPyme': {
        '20': 0,
        '30': 0,
        '40': 0,
        '50': 0,
        '60': 0,
        '70': 0,
        '80': 0,
    },
    'Particular': {
        '20': 0,
        '30': 0,
        '40': 0,
        '50': 0,
        '60': 0,
        '70': 0,
        '80': 0,
    },
    'Transporte de Personal': {
        '20': 0,
        '30': 0,
        '40': 0,
        '50': 0,
        '60': 0,
        '70': 0,
        '80': 0,
    }
}

def roundup(x):
    return int(math.ceil(x / 10.0)) * 10

def radar_factory(num_vars, frame='circle'):
    """Create a radar chart with `num_vars` axes.

    This function creates a RadarAxes projection and registers it.

    Parameters
    ----------
    num_vars : int
        Number of variables for radar chart.
    frame : {'circle' | 'polygon'}
        Shape of frame surrounding axes.

    """
    # calculate evenly-spaced axis angles
    theta = np.linspace(0, 2*np.pi, num_vars, endpoint=False)

    class RadarAxes(PolarAxes):

        name = 'radar'
        # use 1 line segment to connect specified points
        RESOLUTION = 1

        def __init__(self, *args, **kwargs):
            super().__init__(*args, **kwargs)
            # rotate plot such that the first axis is at the top
            self.set_theta_zero_location('N')

        def fill(self, *args, closed=True, **kwargs):
            """Override fill so that line is closed by default"""
            return super().fill(closed=closed, *args, **kwargs)

        def plot(self, *args, **kwargs):
            """Override plot so that line is closed by default"""
            lines = super().plot(*args, **kwargs)
            for line in lines:
                self._close_line(line)

        def _close_line(self, line):
            x, y = line.get_data()
            # FIXME: markers at x[0], y[0] get doubled-up
            if x[0] != x[-1]:
                x = np.concatenate((x, [x[0]]))
                y = np.concatenate((y, [y[0]]))
                line.set_data(x, y)

        def set_varlabels(self, labels):
            self.set_thetagrids(np.degrees(theta), labels)

        def _gen_axes_patch(self):
            # The Axes patch must be centered at (0.5, 0.5) and of radius 0.5
            # in axes coordinates.
            if frame == 'circle':
                return Circle((0.5, 0.5), 0.5)
            elif frame == 'polygon':
                return RegularPolygon((0.5, 0.5), num_vars,
                                      radius=.5, edgecolor="k")
            else:
                raise ValueError("unknown value for 'frame': %s" % frame)

        def _gen_axes_spines(self):
            if frame == 'circle':
                return super()._gen_axes_spines()
            elif frame == 'polygon':
                # spine_type must be 'left'/'right'/'top'/'bottom'/'circle'.
                spine = Spine(axes=self,
                              spine_type='circle',
                              path=Path.unit_regular_polygon(num_vars))
                # unit_regular_polygon gives a polygon of radius 1 centered at
                # (0, 0) but we want a polygon of radius 0.5 centered at (0.5,
                # 0.5) in axes coordinates.
                spine.set_transform(Affine2D().scale(.5).translate(.5, .5)
                                    + self.transAxes)
                return {'polar': spine}
            else:
                raise ValueError("unknown value for 'frame': %s" % frame)

    register_projection(RadarAxes)
    return theta


def read_data(path='./data/clients.csv'):
    with open(path) as csv_file:
        reader = csv.reader(csv_file, delimiter=',')
        i = 0
        for row in reader:
            if i == 0:
                i += 1
                continue
            segment = row[10]
            age = int(row[1])
            age_group = str(roundup(age))
            data_dict[segment][age_group] += 1
        csv_file.close()
    print(data_dict)

def example_data():
    data = [
        segments,
        ('Basecase', [
            [data_dict['Taxi']['20'], data_dict['Plataforma']['20'], data_dict['Corporativo']['20'], data_dict['MiPyme']['20'], data_dict['Particular']['20'],data_dict['Transporte de Personal']['20']],
            [data_dict['Taxi']['30'], data_dict['Plataforma']['30'], data_dict['Corporativo']['30'], data_dict['MiPyme']['30'], data_dict['Particular']['30'],data_dict['Transporte de Personal']['30']],
            [data_dict['Taxi']['40'], data_dict['Plataforma']['40'], data_dict['Corporativo']['40'], data_dict['MiPyme']['40'], data_dict['Particular']['40'],data_dict['Transporte de Personal']['40']],
            [data_dict['Taxi']['50'], data_dict['Plataforma']['50'], data_dict['Corporativo']['50'], data_dict['MiPyme']['50'], data_dict['Particular']['50'],data_dict['Transporte de Personal']['50']],
            [data_dict['Taxi']['60'], data_dict['Plataforma']['60'], data_dict['Corporativo']['60'], data_dict['MiPyme']['60'], data_dict['Particular']['60'],data_dict['Transporte de Personal']['60']],
            [data_dict['Taxi']['70'], data_dict['Plataforma']['70'], data_dict['Corporativo']['70'], data_dict['MiPyme']['70'], data_dict['Particular']['70'],data_dict['Transporte de Personal']['70']],
            [data_dict['Taxi']['80'], data_dict['Plataforma']['80'], data_dict['Corporativo']['80'], data_dict['MiPyme']['80'], data_dict['Particular']['80'],data_dict['Transporte de Personal']['80']]]),
    ]
    return data


if __name__ == '__main__':
    read_data()
    N = 6
    theta = radar_factory(N, frame='polygon')

    data = example_data()
    spoke_labels = data.pop(0)

    fig, axes = plt.subplots(figsize=(N, N), nrows=2, ncols=2,
                             subplot_kw=dict(projection='radar'))
    fig.subplots_adjust(wspace=0.25, hspace=0.20, top=0.85, bottom=0.05)

    colors = ['b', 'r', 'g', 'm', 'y', 'c', 'k']
    # Plot the four cases from the example data on separate axes
    for ax, (title, case_data) in zip(axes.flat, data):
        ax.set_rgrids([20, 40, 60, 80, 100])
        ax.set_title(title, weight='bold', size='medium', position=(0.5, 1.1),
                     horizontalalignment='center', verticalalignment='center')
        for d, color in zip(case_data, colors):
            ax.plot(theta, d, color=color)
            ax.fill(theta, d, facecolor=color, alpha=0.25)
        ax.set_varlabels(spoke_labels)

    # add legend relative to top-left plot
    ax = axes[0, 0]
    labels = ('11-20 Años', '21-30 Años', '31-40 Años', '41-50 Años', '51-60 Años', '61-70 Años', '71-80 Años')
    legend = ax.legend(labels, loc=(0.9, .95),
                       labelspacing=0.1, fontsize='small')

    fig.text(0.5, 0.965, 'Perfil de edad por segmento en clientes',
             horizontalalignment='center', color='black', weight='bold',
             size='large')

    plt.show()