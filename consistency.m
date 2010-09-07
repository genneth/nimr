function data = consistency

warning off MATLAB:xlsread:Mode;

% the order of domains is always the same
domains = {'p3', 'pMN', 'intermediate', 'dorsal'};
colours = [87 157 28; 197 0 11; 102 102 102; 0 102 204] ./ 255;

% data is taken by transverse slices, which are 6 microns thick
% contains: mitotic index, progenitor and neuron numbers and cell area
data20100726 = xlsread('data/data 26072010/mit index prog neurons.xls', ...
                       'Sheet1', '', 'basic');

ts1 = data20100726(1, 2:13);

mitindex.av = data20100726(4:7, 2:13);
mitindex.sd = data20100726(10:13, 2:13);

neurons.av = data20100726(20:23, 2:13);
neurons.sd = data20100726(25:28, 2:13);

progenitors.av = data20100726(35:38, 2:13);
progenitors.sd = data20100726(40:43, 2:13);

plot_domains(ts1, progenitors.av, progenitors.sd, ...
    domains, colours, 'number of progenitors', '');

cellarea.av = data20100726(50:53, 2:13);
cellarea.sd = data20100726(55:58, 2:13);

plot_domains(ts1, cellarea.av, cellarea.sd, ...
    domains, colours, 'cell area (micrometres^2)', '');

% plot_domains(ts1, progenitors.av + neurons.av, ...
%     sqrt(progenitors.sd.^2 + neurons.av .^ 2), ...
%     domains, colours, 'number of cells (progenitor and neuron)');

plot_domains(ts1, ...
    (progenitors.av + neurons.av) ./ ...
        repmat([1; 4; 4; 7], 1, numel(ts1)), ...
    sqrt(progenitors.sd.^2 + neurons.av .^ 2) ./ ...
        repmat([1; 4; 4; 7], 1, numel(ts1)), ...
    domains, colours, 'proportion of final cells', ...
    'growth-fraction');

% domain size measurements, again in transverse planes
DVraw = xlsread('data/DV and AB size Ana.xls', 'Dorsoventral averages', '', 'basic');
dorsoventral.ts = DVraw(5, 1:30);
dorsoventral.av = DVraw(12:15, 1:30);
dorsoventral.sd = DVraw(27:30, 1:30);

plot_domains(dorsoventral.ts, dorsoventral.av, dorsoventral.sd, ...
    domains, colours, '[DV] (micrometer)', ...
    '');

ABraw = xlsread('data/DV and AB size Ana.xls', 'Apicobasal averages', '', 'basic');
apicobasal.ts = ABraw(5, 1:26);
apicobasal.av = ABraw(6:9, 1:26);
apicobasal.sd = ABraw(15:18, 1:26);

plot_domains(apicobasal.ts, apicobasal.av, apicobasal.sd, ...
    domains, colours, '[AB] (micrometer)', ...
    '');

% mitotic index measured by PH3 expression
PH3raw = xlsread('data/ph3 mitotic index.xls', '', '', 'basic');
ph3.ts = PH3raw(1, 1:11);
ph3.av = PH3raw(3:6, 1:11);
ph3.sd = PH3raw(8:11, 1:11);

% cell depth check: b = [AB] [DV] s / p a
dorsoventral.interp = interp1(dorsoventral.ts, dorsoventral.av', ts1, 'linear')';
apicobasal.interp   = interp1(apicobasal.ts, apicobasal.av', ts1, 'linear')';
celldepth = apicobasal.interp .* dorsoventral.interp .* 6 ./ (progenitors.av .* cellarea.av);
plot_domains(ts1, celldepth, [], ...
    domains, colours, 'cell depth (micrometer)', ...
    'cell-depth');

fh = figure;
ah = newplot(fh);
hist(ah, reshape(celldepth, 1, numel(celldepth)));
xlabel(ah, 'cell depth (micrometres)', 'FontName', 'Palatino', 'FontWeight', 'bold', 'FontSize', 9);
set(ah, 'FontName', 'Palatino', 'FontSize', 8)
set(fh, 'PaperPositionMode', 'auto');
set(fh, 'PaperUnits', 'inches');
set(fh, 'PaperSize', [3 2.2]);
print(fh, '-depsc2', '-painters', 'cell-depth-hist.eps');

% division rate and progenitor proportions:
% \rho / \lambda \tau_mitosis = p / m s [DV]
% we can do this with either PH3 measurement of mitosis, or by location
% alone
% ph3:
progenitors.interp  = interp1(ts1, progenitors.av', ph3.ts, 'linear')';
dorsoventral.interp = interp1(dorsoventral.ts, dorsoventral.av', ph3.ts, 'linear')';
progenitors.sdinterp  = interp1(ts1, progenitors.sd', ph3.ts, 'linear')';
dorsoventral.sdinterp = interp1(dorsoventral.ts, dorsoventral.sd', ph3.ts, 'linear')';
rhoph3taus = (ph3.av) ./ (progenitors.interp / 6 ./ dorsoventral.interp);
rhoph3taussd = rhoph3taus .* sqrt(...
    (progenitors.sdinterp ./ progenitors.interp).^2 + ...
    (ph3.sd ./ ph3.av).^2 + ...
    (dorsoventral.sdinterp ./ dorsoventral.interp).^2);

plot_domains(ph3.ts, rhoph3taus, [], ...
    domains, colours, '\rho \tau^{PH3}_{mit.} / \tau_{cycle}', ...
    'rhoph3taus');

plot_domains(ts1, mitindex.av, [], ...
    domains, colours, '\rho \tau_{mit.} / \tau_{cycle}', ...
    'rhotaus');

data.ts1 = ts1;
data.mitindex = mitindex;
data.progenitors = progenitors;
data.neurons = neurons;
data.cellarea = cellarea;
data.dorsoventral = dorsoventral;
data.apicobasal = apicobasal;
data.ph3mitindex = ph3;

end
