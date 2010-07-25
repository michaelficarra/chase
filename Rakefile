task :default => [:lexer,:parser]

desc 'Generate lexer with rexical'
task :lexer do
	sh 'rex *.rex --stub'
end

desc 'Generate parser with racc'
task :parser do
	sh 'racc -SEv --debug -e "/usr/bin/env ruby" *.racc'
end

desc 'Clean up generated files and files output during debugging'
task :clean do
	sh 'rm *.output *.rex.rb *.tab.rb'
end

desc 'Test the generated parser against the sample program'
task :test do
	sh 'ruby *.tab.rb *.fol'
end
