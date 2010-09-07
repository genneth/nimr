function plot_domains(T, Y, YE, labels, colours, yaxislabel, file)

fh = figure;
ah = newplot(fh);

n = numel(labels);

set(ah, 'NextPlot', 'add');

for i=1:n
    if numel(YE)==0
        plot(ah, T, Y(i,:), 's-', 'LineWidth', 2, 'Color', colours(i,:));
    else
        errorbar(ah, T, Y(i,:), YE(i,:), 's-', 'LineWidth', 2, 'Color', colours(i,:));
    end
    drawnow;
end

set(ah, 'XLim', [min(T) max(T)]);
set(ah, 'YLim', [0 max(max(Y))]);
legend(ah, labels);
xlabel(ah, 'time (hours)', 'FontName', 'Palatino', 'FontWeight', 'bold', 'FontSize', 9);
ylabel(ah, yaxislabel, 'FontName', 'Palatino', 'FontWeight', 'bold', 'FontSize', 9);
set(ah, 'FontName', 'Palatino', 'FontSize', 8);

set(fh, 'PaperPositionMode', 'auto');
set(fh, 'PaperUnits', 'inches');
set(fh, 'PaperSize', [3 2.2]);

% set(ah, 'Units', 'normalized');
% margins = get(ah, 'TightInset');
% set(ah, 'OuterPosition', [0 0 1 1]);
% set(ah, 'Position', [0 0 1 1] + margins * 1.5 * [1 0 0 0; 0 1 0 0; -1 0 -1 0; 0 -1 0 -1]');

if ~strcmp(file, '')
    print(fh, '-depsc2', '-painters', strcat(file, '.eps'));
end

end
