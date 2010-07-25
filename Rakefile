task :default => [:lexer,:parser]

desc 'Generate lexer with rexical'
task :lexer do
	sh 'rex first-order-logic.rex --stub'
end

desc 'Generate parser with racc'
task :parser do
	sh 'racc -SEv --debug -e "/usr/bin/env ruby" first-order-logic.racc'
end
