// ECharts'i kohandatud kooste: ainult need osad, mida docs/index.html kasutab.
// Täiskooste on ~1 MB, see ~40% sellest. Kui lehele lisandub uus graafikutüüp või
// komponent (nt dataZoom, pie), lisa see siia ja ehita fail uuesti:
//
//   npm install --no-save echarts@5.5.0 esbuild
//   npx esbuild tools/echarts-kooste.mjs --bundle --minify --format=iife \
//       --global-name=echarts --legal-comments=none --outfile=docs/lib/echarts.min.js
//
// Puuduva osa tunneb ära brauseri konsooli veast "Series ... not exists" või
// "Component ... not exists".
import * as echarts from 'echarts/core';
import { LineChart, BarChart, ScatterChart, HeatmapChart } from 'echarts/charts';
import { GridComponent, TooltipComponent, LegendComponent, MarkLineComponent, MarkPointComponent, VisualMapComponent } from 'echarts/components';
import { SVGRenderer } from 'echarts/renderers';

echarts.use([LineChart, BarChart, ScatterChart, HeatmapChart,
  GridComponent, TooltipComponent, LegendComponent, MarkLineComponent, MarkPointComponent, VisualMapComponent,
  SVGRenderer]);

export * from 'echarts/core';
